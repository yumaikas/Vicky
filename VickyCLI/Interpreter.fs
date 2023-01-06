module Interpreter

open System
open System.IO

type ParseFailureInfo = { message: string; line: int; column: int; from: string }

exception ParseFailed of ParseFailureInfo
exception ArgumentError of string
exception CannotEvalError of string
exception TypeError of string
exception InvalidNameException of string

type PositionAnnotatedCode =
    val code: char list
    val line: int
    val column: int
    val from: string
    new(newCode: char list, newLine: int, newColumn: int, newFrom: string) =
        {code = newCode; line = newLine; column = newColumn; from = newFrom}

let posFromSource (code: char list) (from: string) =
    PositionAnnotatedCode(code, 0, 0, from)
    
let readN (n: int) (code: PositionAnnotatedCode) =
    let mutable newColumn = code.column
    let mutable newLine = code.line
    let mutable newCode = code.code

    for i = 1 to n do
        if not newCode.IsEmpty then
            if newCode.Head = '\n' then
                newColumn <- 0
                newLine <- newLine + 1
            else
                newColumn <- newColumn + 1
            newCode <- newCode.Tail
    PositionAnnotatedCode(newCode, newLine, newColumn, code.from)

let failedAt (message: string) (pos: PositionAnnotatedCode) =
    ParseFailed { 
        message=message;
        column = pos.column;
        line = pos.line;
        from = pos.from;
    }


type Special =
    | Add
    | Mult
    | Div
    | Sub
    | And
    | Or

type Symbol = string

type VickyValue =
    | Str of string
    | Num of double
    | Boolean of bool
    | Dict of Map<VickyValue, VickyValue> // Might change to Dictionary later
    | List of VickyValue list 
    | Fn of VickyFunc
    | Symbol of Symbol
    | Null


and VickyTerm =
    | Str of string
    | Num of double
    | Boolean of bool
    | Symbol of Symbol
    | Quote of VickyTerm
    | Func of DefinedFunc
    | List of VickyTerm list

    // Feels like a runtime construct
    | ResolvedSymbol of Symbol * VickyValue
    // Special forms
    | Define of Symbol * VickyTerm
    | If of VickyTerm * VickyTerm * VickyTerm
    | When of VickyTerm * VickyTerm
    | While of VickyTerm * VickyTerm list

and NativeFunc = 
    val fullName: string // Needs to be a fully qualified name. Might need to reify that idea laster
    val fn: VickyValue list -> VM -> Env -> VickyValue * Env
    new(name: string, fn0: VickyValue list -> VM -> Env -> VickyValue * Env) = 
        { fullName = name; fn = fn0 }
    override this.Equals other =
        match other with
        | :? NativeFunc as nf -> nf.fullName.Equals(this.fullName)
        | _ -> false
    override this.GetHashCode() =
        this.fullName.GetHashCode()
    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? NativeFunc as nf -> this.fullName.CompareTo(nf.fullName)
            | _ -> -1

and DefinedFunc =
    val fullName: string
    val args: Symbol list
    val body: VickyTerm list list
    new(name: string, argsIn: Symbol list, bodyIn: VickyTerm list list) = 
        { fullName = name; args = argsIn; body = bodyIn }

    override this.ToString() =
        sprintf "FN:[%A %A %A]" this.fullName this.args this.body

    override this.Equals other =
        match other with
        | :? DefinedFunc as nf -> nf.fullName.Equals(this.fullName)
        | _ -> false
    override this.GetHashCode() =
        this.fullName.GetHashCode()
    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? DefinedFunc as nf -> this.fullName.CompareTo(nf.fullName)
            | _ -> -1

and VickyFunc = 
    | Native of NativeFunc
    | Defined of DefinedFunc

and Env = Map<Symbol, VickyValue>
and StackFrame =
    val args: VickyValue list
    val mutable currentVariables: Map<Symbol, VickyValue>
    val mutable returnValue: VickyValue option
    new(argsIn: (Symbol * VickyValue) list) = { 
        args = argsIn |> List.map (fun(_,v) -> v);
        currentVariables = Map<Symbol, VickyValue>(argsIn);
        returnValue = None;
    }

and VM = 
    val mutable env: Env
    val mutable stack: StackFrame list
    new(env0: Env, stack0: StackFrame list) = { env = env0; stack = stack0 }

let isInt (d: double) =
    Math.Abs(d % 1.0) = 0

let rec formatValue (value: VickyValue): string =
    match value with
    | VickyValue.Str(s) -> "\"" + s + "\""
    | VickyValue.Num(d) when Math.Abs(d % 1.0) < (Double.Epsilon * 100.0) -> sprintf "%0.0f" d
    | VickyValue.Num(d) -> sprintf "%f" d
    | VickyValue.Boolean(true) -> "true"
    | VickyValue.Boolean(false) -> "false"
    | VickyValue.Dict(d) -> 
        $"""{{{ String.Join(" ", (Map.map (fun k v -> $"{formatValue k} {formatValue v}"  ) d)) }}}"""
    | VickyValue.List(l) ->
        $"""({ String.Join(" ", List.map formatValue l) })"""
    | VickyValue.Symbol(s) -> s
    | VickyValue.Fn(Native(f)) -> $"<{f.fullName}>"
    | VickyValue.Fn(Defined(f)) -> $"<{f.fullName}>"
    | Null -> "null"

let isNum(c: char) = '0' <= c && c <= '9'

let listToString (cs: char list) = cs |> Array.ofList |> String

let isSpace(c: char) = 
    match c with
    | ' ' | '\t'  | '\r' | '\n' -> true
    | _ -> false

let isLetter(c: char) =
    match c with 
    | c when 'a' <= c && c  <= 'z' -> true
    | c when 'A' <= c && c <= 'Z' -> true
    | _ -> false

let isAlNum(c: char) = isLetter c || isNum c

let isIdent(c: char) =
    match c with 
    | c when isAlNum c -> true
    | '+' | '-' | '/' | '*' | '$' | '%' | '@' 
    | '!' | '?' | '^' | '&' | '<' | '>' | ',' | '.' -> true
    | _ -> false

let assertOrParseError (pos: PositionAnnotatedCode) (isValid: bool) (message: string) =
    if not isValid then
        raise (
            ParseFailed { 
            message=message;
            column = pos.column;
            line = pos.line;
            from = pos.from;
        })
    else
        ()

let splitOnChange<'a> (pred: 'a -> bool) (l: 'a list): 'a list * 'a list =
    let head = List.takeWhile pred l
    let tail = List.skipWhile pred l
    head, tail

let isValidTermBorder(c: char list) = 
    match c with
    | [] -> true
    | c :: _ when isSpace c -> true
    | ')' :: _ -> true
    | _ -> false

let handleStringEscapesSeqs (s: string) =
    s.Replace("\\n", "\n").Replace("\\r", "\r").Replace("\\t", "\t")

let parseString(input: PositionAnnotatedCode) =
    let (str, rem) = splitOnChange<char> (fun c -> c <> '"') input.code
    let output = readN str.Length input
    let validStop =
        match rem with
        | '"' :: tail when isValidTermBorder(tail) -> true
        | _ -> false
    assertOrParseError output validStop "Unterminated string!"
    VickyTerm.Str((listToString str) |> handleStringEscapesSeqs), (readN 1 output)


let parseNumber(input: PositionAnnotatedCode) =
    let (num, rest) = (splitOnChange isNum) input.code
    let output = readN (num.Length) input
    assertOrParseError output (isValidTermBorder rest) "Invalid characters at end of number!"
    let retVal = Num(num |> listToString |> Double.Parse)
    retVal, output


let parseSymbol(input: PositionAnnotatedCode) =
    assertOrParseError input (isIdent input.code.Head) "Tried to call parseSymbol with non identifier starting character!"
    let (symbol, rest) = splitOnChange isIdent input.code 
    let output = readN symbol.Length input
    assertOrParseError output (isValidTermBorder rest) (sprintf "Invalid characters at end of symbol: %A" rest)
    let retVal = VickyTerm.Symbol(symbol |> listToString)
    retVal, output

let isArg (t: VickyTerm) =
    match t with
    | VickyTerm.Symbol(_) -> true
    | _ -> false

let isCallOrOp (t: VickyTerm) =
    match t with
    | VickyTerm.List(VickyTerm.Symbol(_)::_) -> true
    | _ -> false


let argify (terms: VickyTerm list) =
    seq {
        for t in terms do
            match t with
            | VickyTerm.Symbol(s) -> s
            | _ -> raise (Exception (sprintf "Called argify incorrectly with %A!" terms))
    } |> List.ofSeq

let embody (terms: VickyTerm list): VickyTerm list list =
    seq {
        for t in terms do
            match t with
            | VickyTerm.List(s) -> s
            | _ -> raise (Exception (sprintf "Called embody incorrectly with %A!" terms))
    } |> List.ofSeq

let mutable anonFnCounter = 0

let genAnonName = 
    let ret = sprintf "ANON:[%A]" anonFnCounter 
    anonFnCounter <- anonFnCounter + 1
    ret

let parseFnLiteral(terms: VickyTerm list) (pos: PositionAnnotatedCode) =
    match terms with
    | VickyTerm.List(l) :: body 
        when List.forall isArg l && List.forall isCallOrOp body
        -> DefinedFunc(genAnonName, argify l, (embody body))
    // TODO: Better error messages here
    | _ -> raise (failedAt (sprintf "Invalid fn %A" terms) pos)

let parseDefLiteral(terms: VickyTerm list) (pos: PositionAnnotatedCode) =
    match terms with
    | [VickyTerm.Symbol(s); value] -> Define(s, value)
    // TODO: Better error messages here
    | _ -> raise (failedAt (sprintf "Invalid def %A" terms) pos)

let parseIfForm(terms: VickyTerm list) (pos: PositionAnnotatedCode) =
    // RESUME: Build out IF form here
    match terms with
    | [cond; true_branch; false_branch] -> If(cond, true_branch, false_branch)
    | [cond; true_branch; ] -> When(cond, true_branch)
    // TODO: Better error messages here
    | _ -> raise (failedAt (sprintf "Invalid if expression %A" terms) pos)

let parseWhileForm(terms: VickyTerm list) (pos: PositionAnnotatedCode) =
    match terms with
    | cond :: rest when rest.Length > 0 -> While(cond, rest)
    | _ -> raise (failedAt (sprintf "Invalid if expression %A" terms) pos)

let rec parseTerms(baseInput: PositionAnnotatedCode) (seekingDelim: char option): VickyTerm list * PositionAnnotatedCode =
    let mutable input = baseInput 
    let mutable keepGoing = true 
    let mutable results: VickyTerm list = [] 

    while keepGoing do
        match input.code with
        | c :: rest when isSpace c -> 
            input <- readN 1 input
        | c :: _ when isNum c -> 
            let (value, rem) = parseNumber input
            input <- rem
            results <- value :: results
        | c :: _ when isIdent c ->
            let (value, rem) = parseSymbol input
            input <- rem
            results <- value :: results
        | '\'' :: '(' :: _ ->
            let (value, rem) = parseTerms (readN 2 input) (Some ')')
            input <- rem
            results <- Quote(List(value)) :: results
        | '\'' :: c :: _ when isNum c -> 
            let (value, rem) = parseNumber (readN 1 input)
            input <- rem
            results <- Quote(value) :: results
        | '\'' :: '"' :: _ ->
            let (value, rem) = parseString (readN 2 input)
            input <- rem
            results <- Quote(value) :: results
        | '\'' :: c :: _ when isIdent c -> 
            let (value, rem) = parseSymbol (readN 1 input)
            input <- rem
            results <- Quote(value) :: results
        | '"' :: rest -> 
            let (value, rem) = parseString (readN 1 input)
            input <- rem
            results <- value :: results
        | '(' :: rest -> 
            let (value, rem) = parseTerms (readN 1 input) (Some ')')
            input <- rem
            results <- (VickyTerm.List value) :: results
        | c :: _ when seekingDelim.IsSome && seekingDelim.Value = c -> 
            keepGoing <- false
            input <- readN 1 input
        | [] when seekingDelim.IsSome ->
            raise (failedAt (sprintf "Missing delimiter %A" seekingDelim.Value) input)
        | [] when seekingDelim.IsNone -> 
            input <- input
            keepGoing <- false
        | c -> raise (failedAt (sprintf "Unexpected character %A" c) input)

    results <- List.rev results
    match results with
    | VickyTerm.Symbol("fn") :: rest -> [VickyTerm.Func (parseFnLiteral rest baseInput)], input
    | VickyTerm.Symbol("def") :: rest -> [(parseDefLiteral rest baseInput)], input
    | VickyTerm.Symbol("if") :: rest -> [(parseIfForm rest baseInput)], input
    | VickyTerm.Symbol("while") :: rest -> [(parseWhileForm rest baseInput)], input
    | VickyTerm.Symbol(_) :: _ -> results, input
    | VickyTerm.List(VickyTerm.Func(fn) :: _) :: _ -> results, input
    | [] -> raise  (failedAt "Empty expression!" baseInput)
    | terms -> terms, input


let parse (input: PositionAnnotatedCode) = 
    let mutable input = input
    let mutable results = []

    while not input.code.IsEmpty do
        match input.code with
        | ' ' :: _ | '\t' :: _ | '\r' :: _ | '\n' :: _ -> 
            input <- readN 1 input
        | '(' :: _ -> 
            let (term, rem) = parseTerms (readN 1 input) (Some ')')
            input <- rem
            results <- term :: results
        | '\'' :: '(' :: _ ->
            let (term, rem) = parseTerms (readN 2 input) (Some ')')
            input <- rem
            results <- [Quote(VickyTerm.List(term))] :: results
        | c ->
            let (term, rem) = parseTerms input None
            input <- rem
            results <- term :: results
    List.rev results


let fnAdd (args: VickyValue list) (vm: VM) (env: Env) =
    let mutable sum = 0.0
    for a in args do
        match a with
        | VickyValue.Num n -> sum <- n + sum
        | v -> raise (TypeError (sprintf "Unexpected value of type %A" v.GetType))
    VickyValue.Num(sum), env

let fnSubtract (args: VickyValue list) (vm: VM) (env: Env) =
    let mutable sum = 
        match args with
        | VickyValue.Num firstValue :: _ -> firstValue
        | head :: _ -> raise (TypeError (sprintf "Cannot subtract starting with non-number %A" head))
        | _ -> raise (ArgumentError("Cannot subtract empty list"))

    for a in args.Tail do
        match a with
        | VickyValue.Num n -> sum <- sum - n
        | v -> raise (TypeError (sprintf "Unexpected value of type %A" v.GetType))
    VickyValue.Num(sum), env

let fnDivide (args: VickyValue list) (vm: VM) (env: Env) =
    let mutable sum = 
        match args with
        | VickyValue.Num firstValue :: _ -> firstValue
        | head :: _ -> raise (TypeError (sprintf "Cannot divide starting with non-number %A" head))
        | _ -> raise (ArgumentError("Cannot divide empty list"))

    for a in args.Tail do
        match a with
        | VickyValue.Num n -> sum <- sum / n
        | v -> raise (TypeError (sprintf "Unexpected value of type %A" v.GetType))
    VickyValue.Num(sum), env

let fnMultiply (args: VickyValue list) (vm: VM) (env: Env) =
    let mutable sum = 1.0
    for a in args do
        match a with
        | VickyValue.Num n -> sum <- n * sum
        | v -> raise (TypeError (sprintf "Unexpected value of type %A" v.GetType))
    VickyValue.Num(sum), env

let fnList (args: VickyValue list) (vm: VM) (env: Env) =
    VickyValue.List(args), env

let fnDict (args: VickyValue list) (vm: VM) (env: Env) =
    if (args.Length % 2) <> 0 then
        raise (ArgumentError "Called dict with an uneven number of arguments!")

    List.chunkBySize 2 args 
    |> List.map (fun [a; b] -> a, b) 
    |> Map.ofList
    |> VickyValue.Dict, env

let fnGet (args: VickyValue list) (vm: VM) (env: Env) = 
    match args with
    | VickyValue.Dict(m) :: key :: _ when m.ContainsKey(key) ->
        m[key], env
    | VickyValue.List(l) :: VickyValue.Num(idx) :: _ when idx < l.Length ->
        List.item (int idx) l, env
    | VickyValue.List(l) :: VickyValue.Num(idx) :: _ when idx > l.Length ->
        raise (ArgumentError "Index too large for list!")
    | _ -> 
        VickyValue.Null, env
let fnPut (args: VickyValue list) (vm: VM) (env: Env): VickyValue * Env =
    match args with
    | VickyValue.Dict(m) :: key :: value :: _ when m.ContainsKey(key) ->
        VickyValue.Dict(m.Add(key, value)), env
    | VickyValue.List(l) :: VickyValue.Num(idx) :: value :: _ when isInt idx && idx < l.Length ->
        VickyValue.List(List.updateAt (int idx) value l), env
    | VickyValue.List(l) :: VickyValue.Num(idx) :: _ when idx > l.Length ->
        raise (ArgumentError "Index too large for list!")
    | _ -> 
        VickyValue.Null, env


let fnStrSplit(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | [VickyValue.Str(s)] -> 
        VickyValue.List(s.Split(' ') |> Seq.map (fun s -> VickyValue.Str(s)) |> List.ofSeq), env
    | [VickyValue.Str(s); VickyValue.Str(delim)] -> 
        VickyValue.List(s.Split(delim) |> Seq.map (fun s -> VickyValue.Str(s)) |> List.ofSeq), env
    | v -> raise (TypeError (sprintf "Invalid arguments to str/split %A" args))

let fnStr(args: VickyValue list) (vm: VM) (env: Env) =
    VickyValue.Str(String.Join("", (List.map 
    (fun v ->
        match v with 
        | VickyValue.Str(s) -> s
        | v -> formatValue v
    ) args))), env

let fnStrJoin(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | VickyValue.Str(sep) :: VickyValue.List(parts) :: _ ->
        VickyValue.Str(String.Join(sep, (List.map 
        (fun v ->
            match v with 
            | VickyValue.Str(s) -> s
            | v -> formatValue v
        ) parts))), env
    | v -> raise (TypeError (sprintf "Invalid arguments to str/join %A" args))



let nativeFn name fn =
    VickyValue.Fn(VickyFunc.Native(NativeFunc(name, fn)))


let resolveSymbol (symbol: Symbol) (frame: StackFrame) (env: Env) =
    let (key) = symbol
    if frame.currentVariables.ContainsKey(key) then
        ResolvedSymbol(key, frame.currentVariables[key])
    elif env.ContainsKey(symbol) then
        ResolvedSymbol(key, env[symbol])
    else
        raise (InvalidNameException (sprintf "Couldn't find value for name %A" symbol))

let emptyFrame = StackFrame([])

let rec resolveTerm (vm: VM) (term: VickyTerm)  =
    let resolve = resolveTerm vm
    match term with
    | Symbol(s) -> 
        if vm.stack.IsEmpty then
            resolveSymbol s emptyFrame vm.env
        else
            resolveSymbol s vm.stack.Head vm.env
    // Each of the special forms gets resolved through here
    | Define(s, t) -> Define(s, resolveTerm vm t)
    | If(cond, true_form, false_form) -> If(resolve cond, resolve true_form, resolve false_form)
    | When(cond, true_form) -> When(resolve cond, resolve true_form)
    | While(cond, body) -> While(resolve cond, List.map resolve body)
    | VickyTerm.List(l) -> VickyTerm.List (List.map resolve l)
    | term -> term

let rec termToValue (vm: VM) (inQuote: bool) (term: VickyTerm)=
    match term with
    | Boolean(b) -> VickyValue.Boolean(b)
    | Num(n) -> VickyValue.Num(n)
    | Str(s) -> VickyValue.Str(s)
    | List(ResolvedSymbol(_, VickyValue.Fn(VickyFunc.Native n)) :: rest) -> 
        let (ret, newEnv) = n.fn (List.map (termToValue vm inQuote) rest) vm vm.env
        vm.env <- newEnv
        ret
    | List(ResolvedSymbol(_, VickyValue.Dict(map)) :: key :: _) -> 
        let resolvedKey = (termToValue vm inQuote) key
        match (Map.tryFind resolvedKey map) with
        | Some(value) -> value
        | None -> VickyValue.Null

    | List(l) -> 
        match (List.map (termToValue vm inQuote) l) with
        | VickyValue.Dict(map) :: key :: _ -> 
            match (Map.tryFind key map) with
            | Some(value) -> value
            | None -> VickyValue.Null
        | VickyValue.List(l) :: VickyValue.Num(n) :: _ when isInt n ->
            List.item (int n) l
        | vals -> VickyValue.List(vals)
    | ResolvedSymbol(_, value) -> value
    | Func(fn) -> VickyValue.Fn(VickyFunc.Defined(fn))
    | Quote(value) when not inQuote -> termToValue vm true value
    | Quote(value) -> VickyValue.List([VickyValue.Symbol("quote"); termToValue vm true value])
    | Define(s, t) -> raise (InvalidNameException (sprintf "Define in unexpected location! %A" s))
    | If(_,_,_) | When(_, _) -> raise (InvalidNameException (sprintf "If in unexpected location!"))
    | While(_, _) -> raise (InvalidNameException (sprintf "While in unexpected location!"))
    | Symbol(s) when inQuote -> VickyValue.Symbol(s)
    | Symbol(s) -> raise (InvalidNameException (sprintf "Unresolved symbol!: %A" s))

let rec eval (vm: VM) (exprs: VickyTerm list list) =
    let mutable evalRet = VickyValue.Null
    for expr in exprs do
        let resolved = List.map (resolveTerm vm) expr
        match resolved with
        | ResolvedSymbol(name, VickyValue.Fn(VickyFunc.Native n)) :: rest ->
            let (ret, newEnv) = n.fn (List.map (termToValue vm false) rest) vm vm.env
            vm.env <- newEnv
            evalRet <- ret
        | ResolvedSymbol(name, VickyValue.List(VickyValue.Fn(VickyFunc.Defined fn) :: _)) :: rest ->
            let argVals = (List.map (termToValue vm false) rest)
            let shortLen = min argVals.Length fn.args.Length
            let arglist = (List.zip (List.take shortLen fn.args) (List.take shortLen argVals))
            vm.stack <- StackFrame(arglist) :: vm.stack
            let (ret, newEnv) = eval vm fn.body
            vm.env <- newEnv
            vm.stack <- vm.stack.Tail
            evalRet <- ret
        | ResolvedSymbol(name, VickyValue.Dict(m)) :: ResolvedSymbol(_, key) :: _->
            match (Map.tryFind key m) with
            | Some(value) -> 
                evalRet <- value
            | None -> 
                evalRet <- VickyValue.Null
        | ResolvedSymbol(name, VickyValue.Dict(m)) :: key :: _->
            let resolvedKey = (termToValue vm false) key
            match (Map.tryFind resolvedKey m) with
            | Some(value) -> 
                evalRet <- value
            | None -> 
                evalRet <- VickyValue.Null
        | ResolvedSymbol(name, VickyValue.List(l)) :: VickyTerm.Num(idx) :: _ when isInt idx ->
            evalRet <- List.item (int idx) l
        | ResolvedSymbol(name, VickyValue.List(l)) :: ResolvedSymbol(_, VickyValue.Num(idx)) :: _ when isInt idx -> 
            evalRet <- List.item (int idx) l
        | ResolvedSymbol(name, value) :: _ ->
            evalRet <- value
        | Define(s, t) :: _ -> 
            let definedValue = (termToValue vm false) t
            if vm.stack.Length = 0 then
                vm.env <- vm.env.Add(s, definedValue)
            else
                vm.stack.Head.currentVariables <- vm.stack.Head.currentVariables.Add(s, definedValue)
            evalRet <- definedValue
        | If(cond, true_form, false_form) :: _ ->
            let resolver  = (termToValue vm false)
            let condition = resolver cond
            let form_val =
                match condition with
                | VickyValue.Boolean(false) -> resolver false_form
                | _ -> resolver true_form
            evalRet <- form_val
        | When(cond, true_form) :: _ ->
            let resolver  = (termToValue vm false)
            let condition = resolver cond
            let form_val =
                match condition with
                | VickyValue.Boolean(false) -> VickyValue.Null
                | _ -> resolver true_form
            evalRet <- form_val

        | VickyTerm.List(VickyTerm.Func(fn) ::  _) :: rest ->
            let argVals = (List.map (termToValue vm false) rest)
            let shortLen = min argVals.Length fn.args.Length
            let arglist = (List.zip (List.take shortLen fn.args) (List.take shortLen argVals))
            vm.stack <- StackFrame(arglist) :: vm.stack
            let (ret, newEnv) = eval vm fn.body
            vm.env <- newEnv
            vm.stack <- vm.stack.Tail
            evalRet <- ret
        | VickyTerm.Str(s) :: _ ->
            evalRet <- VickyValue.Str(s)
        | VickyTerm.Num(n) :: _ ->
            evalRet <- VickyValue.Num(n)
        | VickyTerm.Boolean(tf) :: _ ->
            evalRet <- VickyValue.Boolean(tf)
        | VickyTerm.List(VickyTerm.ResolvedSymbol(_, _) :: l) :: VickyTerm.Num(idx) :: _ when isInt idx -> 
            evalRet <- List.item (int idx) (List.map (termToValue vm false) l)
        | VickyTerm.List(VickyTerm.ResolvedSymbol(_, _) :: l) :: VickyTerm.ResolvedSymbol(_, VickyValue.Num(idx)) :: _ when isInt idx -> 
            evalRet <- List.item (int idx) (List.map (termToValue vm false) l)
        | VickyTerm.List(l) :: _ ->
            raise (CannotEvalError (sprintf "Cannot eval %A" l))
        | [elem] ->
            evalRet <- (termToValue vm) false elem
        | elems ->
            evalRet <- VickyValue.List(List.map (termToValue vm false) elems)

    evalRet, vm.env

let evalString(vm: VM) (code: string) (from: string) = 
    let ast = parse (posFromSource (code |> Seq.toList) from)
    let result, _ = eval vm ast
    result

let fnDoFile(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | VickyValue.Str(path) :: _ -> 
        let ast = parse (posFromSource (File.ReadAllText(path) |> List.ofSeq) $"file::{path}")
        eval vm ast
    | _ -> raise (TypeError (sprintf "Invalid arguments to dofile %A" args))

let fnCwd(args: VickyValue list) (vm: VM) (env: Env) =
    VickyValue.Str(Directory.GetCurrentDirectory()), env

let fnCd(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | VickyValue.Str(path) :: _ -> 
        Directory.SetCurrentDirectory(path)
        VickyValue.Str(Directory.GetCurrentDirectory()), env
    | _ -> raise (TypeError (sprintf "Invalid arguments to os/cd %A" args))

let fnOsGetEnv(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | VickyValue.Str(key) :: _ -> 
        let value = Environment.GetEnvironmentVariable(key)
        VickyValue.Str(value), env
    | _ -> raise (TypeError (sprintf "Invalid arguments to os/getenv %A" args))

let fnOsDir(args: VickyValue list) (vm: VM) (env: Env) = 
    match args with
    | VickyValue.Str(path) :: _ -> 
        VickyValue.List(
            List.map (fun s -> VickyValue.Str(s)) (Directory.EnumerateFileSystemEntries(path) |> List.ofSeq )), env
    | [] -> 
        VickyValue.List(
            List.map (fun s -> VickyValue.Str(s)) (Directory.EnumerateFileSystemEntries(".") |> List.ofSeq )), env
    | _ -> raise (TypeError (sprintf "Invalid arguments to os/dir %A" args))



let defaultEnv: Map<string, VickyValue> = (Map.ofList [
    ("+", nativeFn "builtin::+" fnAdd)
    ("*", nativeFn "builtin::*" fnMultiply)
    ("-", nativeFn "builtin::-" fnSubtract)
    ("/", nativeFn "builtin::/" fnDivide)
    ("list", nativeFn "builtin::list" fnList)
    ("dict", nativeFn "builtin::dict" fnDict)
    ("get", nativeFn "builtin::get" fnGet)
    ("put", nativeFn "builtin::put" fnPut)
    ("str/split", nativeFn "str::split" fnStrSplit)
    ("str/join", nativeFn "str::join" fnStrJoin)
    ("str", nativeFn "str::str" fnStr)
    ("os/cwd", nativeFn "os::cwd" fnCwd)
    ("os/cd", nativeFn "os::cd" fnCd)
    ("os/dir", nativeFn "os::dir" fnOsDir)
    ("os/getenv", nativeFn "os::getenv" fnOsGetEnv)
    ("dofile", nativeFn "builtin::dofile" fnDoFile)
    ("true", VickyValue.Boolean(true))
    ("false", VickyValue.Boolean(false))
])
