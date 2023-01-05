module Interpreter

open System

type ParseFailureInfo = { message: string }

exception ParseFailed of ParseFailureInfo
exception TypeError of string
exception InvalidNameException of string
exception CannotExecuteException  of string

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
    | Null

and VickyTerm =
    | Str of string
    | Num of double
    | Boolean of bool
    | Symbol of Symbol
    | ResolvedSymbol of Symbol * VickyValue
    | Define of Symbol * VickyTerm
    | If of VickyTerm * VickyTerm * VickyTerm
    | When of VickyTerm * VickyTerm
    | While of VickyTerm * VickyTerm list
    | Func of DefinedFunc
    | List of VickyTerm list

and NativeFunc = 
    val fullName: string // Needs to be a fully qualified name. Might need to reify that idea laster
    val fn: VickyValue list -> Env -> VickyValue * Env
    new(name: string, fn0: VickyValue list -> Env -> VickyValue * Env) = 
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

let assertOrParseError (isValid: bool) (message: string) =
    if not isValid then
        raise (ParseFailed { message=message})
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

let parseString(code: char list) =
    let (str, rem) = splitOnChange<char> (fun c -> c <> '"') code
    let validStop = '"' = rem.Head && isValidTermBorder(rem.Tail)
    assertOrParseError validStop "Unterminated string!"

    VickyTerm.Str(listToString str), rem.Tail


let parseNumber(code: char list) =
    let (num, rest) = (splitOnChange isNum) code
    assertOrParseError (isValidTermBorder rest) "Invalid characters at end of number!"
    let retVal = Num(num |> listToString |> Double.Parse)
    (retVal, rest)


let parseSymbol(code: char list) =
    assertOrParseError (isIdent  code.Head) "Tried to call parseSymbol with non identifier starting character!"
    let (symbol, rest) = splitOnChange isIdent code 
    assertOrParseError (isValidTermBorder rest) (sprintf "Invalid characters at end of symbol: %A" rest)
    let retVal = VickyTerm.Symbol(symbol |> listToString)
    retVal, rest

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

let parseFnLiteral(terms: VickyTerm list) =
    match terms with
    | VickyTerm.List(l) :: body 
        when List.forall isArg l && List.forall isCallOrOp body
        -> DefinedFunc(genAnonName, argify l, (embody body))
    // TODO: Better error messages here
    | _ -> raise (ParseFailed{message= sprintf "Invalid fn %A" terms})

let parseDefLiteral(terms: VickyTerm list) =
    match terms with
    | [VickyTerm.Symbol(s); value] -> Define(s, value)
    // TODO: Better error messages here
    | _ -> raise (ParseFailed{message=sprintf "Invalid def %A" terms})

let parseIfForm(terms: VickyTerm list) =
    // RESUME: Build out IF form here
    match terms with
    | [cond; true_branch; false_branch] -> If(cond, true_branch, false_branch)
    | [cond; true_branch; ] -> When(cond, true_branch)
    // TODO: Better error messages here
    | _ -> raise (ParseFailed{message=sprintf "Invalid if expression %A" terms})

let parseWhileForm(terms: VickyTerm list) =
    match terms with
    | cond :: rest when rest.Length > 0 -> While(cond, rest)
    | _ -> raise (ParseFailed{message=sprintf "Invalid while expression %A" terms})


let rec parseTerms(code: char list): VickyTerm list * char list =
    let mutable currentCode = code 
    let mutable keepGoing = true 
    let mutable results: VickyTerm list = [] 

    while keepGoing do
        match currentCode with
        | c :: rest when isSpace c -> 
            currentCode <- rest
        | c :: _ when isNum c -> 
            let (value, rem) = parseNumber currentCode
            currentCode <- rem
            results <- value :: results
        | c :: _ when isIdent c ->
            let (value, rem) = parseSymbol currentCode
            currentCode <- rem
            results <- value :: results
        | '"' :: rest -> 
            let (value, rem) = parseString rest
            currentCode <- rem
            results <- value :: results
        | '(' :: rest -> 
            let (value, rem) = parseTerms rest
            currentCode <- rem
            results <- (VickyTerm.List value) :: results
        | ')' :: _ -> 
            currentCode <- currentCode.Tail
            keepGoing <- false
        | [] -> 
            currentCode <- []
            keepGoing <- false
        | c -> raise (ParseFailed {message = sprintf "Unexpected character %A" c})

    results <- List.rev results
    match results with
    | VickyTerm.Symbol("fn") :: rest -> [VickyTerm.Func (parseFnLiteral rest)], currentCode
    | VickyTerm.Symbol("def") :: rest -> [(parseDefLiteral rest)], currentCode
    | VickyTerm.Symbol("if") :: rest -> [(parseIfForm rest)], currentCode
    | VickyTerm.Symbol("while") :: rest -> [(parseWhileForm rest)], currentCode
    | VickyTerm.Symbol(_) :: _ -> results, currentCode
    | VickyTerm.List(VickyTerm.Func(fn) :: _) :: _ -> results, currentCode
    | term :: _ -> [term], currentCode
    | [] -> raise (ParseFailed{message = "Empty expression!"})


let parse (code: char list) = 
    let mutable remaining = code
    let mutable results = []

    while not remaining.IsEmpty do
        match remaining.Head with
        | ' ' | '\t' | '\r' | '\n' -> 
            remaining <- remaining.Tail
        | '(' -> 
            let (term, rem) = parseTerms(remaining.Tail)
            remaining <- rem
            // printf "Chars left: %A" rem
            results <- term :: results
        | c ->
            let (term, rem) = parseTerms(remaining)
            remaining <- rem
            results <- term :: results
    List.rev results

type StackFrame =
    val args: VickyValue list
    val mutable currentVariables: Map<Symbol, VickyValue>
    val mutable returnValue: VickyValue option
    new(argsIn: (Symbol * VickyValue) list) = { 
        args = argsIn |> List.map (fun(_,v) -> v);
        currentVariables = Map<Symbol, VickyValue>(argsIn);
        returnValue = None;
    }

type VM = 
    val mutable env: Env
    val mutable stack: StackFrame list
    new(env0: Env, stack0: StackFrame list) = { env = env0; stack = stack0 }

let fnAdd (args: VickyValue list) (env: Env) =
    let mutable sum = 0.0
    for a in args do
        match a with
        | VickyValue.Num n -> sum <- n + sum
        | v -> raise (TypeError (sprintf "Unexpected value of type %A" v.GetType))
    VickyValue.Num(sum), env

let fnSubtract (args: VickyValue list) (env: Env) =
    let mutable sum = 
        match args with
        | VickyValue.Num firstValue :: _ -> firstValue
        | head :: _ -> raise (TypeError (sprintf "Cannot subtract starting with non-number %A" head))
        | _ -> raise (ParseFailed{message="Cannot subtract empty list"})

    for a in args.Tail do
        match a with
        | VickyValue.Num n -> sum <- sum - n
        | v -> raise (TypeError (sprintf "Unexpected value of type %A" v.GetType))
    VickyValue.Num(sum), env

let fnDivide (args: VickyValue list) (env: Env) =
    let mutable sum = 
        match args with
        | VickyValue.Num firstValue :: _ -> firstValue
        | head :: _ -> raise (TypeError (sprintf "Cannot divide starting with non-number %A" head))
        | _ -> raise (ParseFailed{message="Cannot divide empty list"})

    for a in args.Tail do
        match a with
        | VickyValue.Num n -> sum <- sum / n
        | v -> raise (TypeError (sprintf "Unexpected value of type %A" v.GetType))
    VickyValue.Num(sum), env

let fnMultiply (args: VickyValue list) (env: Env) =
    let mutable sum = 1.0
    for a in args do
        match a with
        | VickyValue.Num n -> sum <- n * sum
        | v -> raise (TypeError (sprintf "Unexpected value of type %A" v.GetType))
    VickyValue.Num(sum), env

let fnList (args: VickyValue list) (env: Env) =
    VickyValue.List(args), env

let fnStrSplit(args: VickyValue list) (env: Env) =
    match args with
    | [VickyValue.Str(s)] -> 
        VickyValue.List(s.Split(' ') |> Seq.map (fun s -> VickyValue.Str(s)) |> List.ofSeq), env
    | [VickyValue.Str(s); VickyValue.Str(delim)] -> 
        VickyValue.List(s.Split(delim) |> Seq.map (fun s -> VickyValue.Str(s)) |> List.ofSeq), env
    | v -> raise (TypeError (sprintf "Invalid arguments to str/split %A" args))

let nativeFn name fn =
    VickyValue.Fn(VickyFunc.Native(NativeFunc(name, fn)))


let defaultEnv: Map<string, VickyValue> = (Map.ofList [
    ("+", nativeFn "builtin::+" fnAdd)
    ("*", nativeFn "builtin::*" fnMultiply)
    ("-", nativeFn "builtin::-" fnSubtract)
    ("/", nativeFn "builtin::/" fnDivide)
    ("list", nativeFn "builtin::list" fnList)
    ("str/split", nativeFn "str::split" fnStrSplit)
    ("true", VickyValue.Boolean(true))
    ("false", VickyValue.Boolean(false))
])

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

let rec termToValue (vm: VM) (term: VickyTerm) =
    match term with
    | ResolvedSymbol(_, value) -> value
    | Boolean(b) -> VickyValue.Boolean(b)
    | Num(n) -> VickyValue.Num(n)
    | Str(s) -> VickyValue.Str(s)
    | List(ResolvedSymbol(_, VickyValue.Fn(VickyFunc.Native n)) :: rest) -> 
        let (ret, newEnv) = n.fn (List.map (termToValue vm) rest) vm.env
        vm.env <- newEnv
        ret
    | List(l) -> VickyValue.List (List.map (termToValue vm) l)
    | Func(fn) -> VickyValue.Fn(VickyFunc.Defined(fn))
    | Define(s, t) -> raise (InvalidNameException (sprintf "Define in unexpected location! %A" s))
    | If(_,_,_) | When(_, _) -> raise (InvalidNameException (sprintf "If in unexpected location!"))
    | While(_, _) -> raise (InvalidNameException (sprintf "While in unexpected location!"))
    | Symbol(s) -> raise (InvalidNameException (sprintf "Unresolved symbol!: %A" s))
    

let rec eval (vm: VM) (exprs: VickyTerm list list) =
    let mutable evalRet = VickyValue.Null
    for expr in exprs do
        let resolved = List.map (resolveTerm vm) expr
        match resolved with
        | ResolvedSymbol(name, VickyValue.Fn(VickyFunc.Native n)) :: rest ->
            let (ret, newEnv) = n.fn (List.map (termToValue vm) rest) vm.env
            vm.env <- newEnv
            evalRet <- ret
        | ResolvedSymbol(name, VickyValue.List(VickyValue.Fn(VickyFunc.Defined fn) :: _)) :: rest ->
            let argVals = (List.map (termToValue vm) rest)
            let shortLen = min argVals.Length fn.args.Length
            let arglist = (List.zip (List.take shortLen fn.args) (List.take shortLen argVals))
            vm.stack <- StackFrame(arglist) :: vm.stack
            let (ret, newEnv) = eval vm fn.body
            vm.env <- newEnv
            vm.stack <- vm.stack.Tail
            evalRet <- ret
        | ResolvedSymbol(name, value) :: _ ->
            evalRet <- value
        | Define(s, t) :: _ -> 
            let definedValue = (termToValue vm) t
            if vm.stack.Length = 0 then
                vm.env <- vm.env.Add(s, definedValue)
            else
                vm.stack.Head.currentVariables <- vm.stack.Head.currentVariables.Add(s, definedValue)
            evalRet <- definedValue
        | If(cond, true_form, false_form) :: _ ->
            let resolver  = (termToValue vm)
            let condition = resolver cond
            let form_val =
                match condition with
                | VickyValue.Boolean(false) -> resolver false_form
                | _ -> resolver true_form
            evalRet <- form_val
        | When(cond, true_form) :: _ ->
            let resolver  = (termToValue vm)
            let condition = resolver cond
            let form_val =
                match condition with
                | VickyValue.Boolean(false) -> VickyValue.Null
                | _ -> resolver true_form
            evalRet <- form_val

        | VickyTerm.List(VickyTerm.Func(fn) ::  _) :: rest ->
            let argVals = (List.map (termToValue vm) rest)
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
        | VickyTerm.List(l) :: _ -> raise (ParseFailed{message="BOOOO!!!"})
        | [VickyTerm.Func(fn)] ->
            printf "%A" fn
        | elems ->
            evalRet <- VickyValue.List(List.map (termToValue vm) elems)

    evalRet, vm.env

let evalString(vm: VM) (code: string) = 
    let ast = parse (code |> Seq.toList)
    let result, _ = eval vm ast
    result

