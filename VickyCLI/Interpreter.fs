module Interpreter

open System
open System.Diagnostics
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
    
let mutable tracing = false

let readN (n: int) (code: PositionAnnotatedCode) =
    let st = new StackTrace(true)
    let depthStr = new String('-', st.FrameCount)
    if tracing then
        printfn "reading %02i chars at %s:%s " n depthStr (st.GetFrame(1).GetMethod().Name)
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
        message = message;
        column = pos.column;
        line = pos.line;
        from = pos.from;
    }

type Symbol = string

type VickyValue =
    | Nil
    | Str of string
    | Num of double
    | Boolean of bool
    | Symbol of Symbol
    | Keyword of Symbol
    | Func of VickyFunc
    | List of VickyValue list
    | Vector of VickyValue array
    | Dict of Map<VickyValue, VickyValue>
    | Atom of VickyValue ref

and NativeFunc = 
    val fullName: string // Needs to be a fully qualified name. Might need to reify that idea laster
    val fn: VickyValue list -> VM -> Env -> VickyValue * Env
    val mutable isMacro: bool
    new(name: string, fn0: VickyValue list -> VM -> Env -> VickyValue * Env) = 
        { fullName = name; fn = fn0; isMacro = false }
    new(name: string, fn0: VickyValue list -> VM -> Env -> VickyValue * Env, isMacro) = 
        { fullName = name; fn = fn0; isMacro = isMacro }
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
    val mutable fullName: string
    val args: Symbol list
    val defEnv: Env
    val body: VickyValue list
    val mutable isMacro: bool
    new(name: string, argsIn: Symbol list, bodyIn: VickyValue list, outerEnv: Env) = 
        { fullName = name; args = argsIn; body = bodyIn; defEnv = outerEnv; isMacro = false }
    new(name: string, argsIn: Symbol list, bodyIn: VickyValue list, outerEnv: Env, isMacro: bool) = 
        { fullName = name; args = argsIn; body = bodyIn; defEnv = outerEnv; isMacro = isMacro }


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

and Env = 
    val mutable values: Map<VickyValue, VickyValue>
    val mutable parent: Env option
    new(startingValues: Map<VickyValue, VickyValue>, parent: Env option ) = {values = startingValues; parent = parent}


and VM = 
    val mutable env: Env
    new(env0: Env) = { env = env0 }

exception VickyExn of VickyValue

let exnM (name: string) (message: string) =
    Map.empty
        .Add(Keyword("type"), Keyword(name))
        .Add(Keyword("message"), Str(message))

let exn (name: string) (message: string) =
    VickyExn (Dict (exnM name message))

let exnData (name: string) (message: string) (data: VickyValue) =
    VickyExn (Dict ((exnM name message).Add(Keyword("data"), data)))

let exnDataOnly (data: VickyValue) =
    VickyExn(
        Dict(Map.empty
            .Add(Keyword("type"), Keyword("error"))
            .Add(Keyword("data"), data)))

let isInt (d: double) =
    Math.Abs(d % 1.0) = 0

let isFunc (value: VickyValue) =
    match value with
    | VickyValue.Func(_) -> true
    | _ -> false

let isList (value: VickyValue) =
    match value with
    | List(_) -> true
    | Nil -> true
    | _ -> false

let asList (value: VickyValue) =
    match value with
    | List(l) -> l
    | Nil -> []
    | _ -> raise (TypeError (sprintf "Tried to cast non-list %A to list!" value))

let addToEnv (env: Env) (key: VickyValue) (value: VickyValue) =
    env.values <- env.values.Add(key, value)
    env


let rec formatValue (value: VickyValue): string =
    match value with
    | Str(s) -> "\"" + s + "\""
    | Num(d) when Math.Abs(d % 1.0) < (Double.Epsilon * 100.0) -> sprintf "%0.0f" d
    | Num(d) -> sprintf "%f" d
    | Boolean(true) -> "true"
    | Boolean(false) -> "false"
    | Dict(d) -> 
        let kvs = Map.toSeq<VickyValue, VickyValue> d
        let entries = Seq.map (fun (e, v) -> $"{formatValue e} {formatValue v}"  ) kvs
        $"""{{{ String.Join(" ", entries) }}}"""
    | List(l) -> $"""({ String.Join(" ", List.map formatValue l) })"""
    | Vector(v) -> $"""[{ String.Join(" ", Seq.map formatValue v) }]"""
    | Symbol(s) -> s
    | Func(Native(f)) -> $"<{f.fullName}>"
    | Func(Defined(f)) -> $"<{f.fullName}>"
    | Keyword(k) -> $":{k}"
    | Nil -> "nil"
    | Atom(value) -> $"@{formatValue value.Value}"

let fnTypeof (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(_) :: [] -> Keyword("string"), env
    | Num(_) :: [] -> Keyword("number"), env
    | Boolean(_) :: [] -> Keyword("bool"), env
    | Dict(_) :: [] -> Keyword("dictionary"), env
    | List(_) :: [] -> Keyword("list"), env
    | Vector(_) :: [] -> Keyword("vector"), env
    | Symbol(_) :: [] -> Keyword("symbol"), env
    | Func(Native(_)) :: [] -> Keyword("func/ffi"), env
    | Func(Defined(_)) :: [] -> Keyword("func"), env
    | Keyword(k) :: [] -> Keyword("keyword"), env
    | Nil :: [] -> Keyword("nil"), env
    | Atom(_) :: [] -> Keyword("atom"), env
    | _ -> raise (exn "type-error" (sprintf "typeof called with invalid args: %s" (formatValue (List args))))

let formatArgs (args: VickyValue list) =
    formatValue(List(args))

let isNum(c: char) = ('0' <= c && c <= '9') || c = '.' || c = '-'
let isFalse(v: VickyValue) =
    match v with
    | Boolean(false) -> true
    | _ -> false

let isNil(v: VickyValue) =
    match v with
    | Nil -> true
    | _ -> false

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
    | '+' | '-' | '/' | '*' | '$' | '%' | '@' | '_' 
    | '!' | '?' | '^' | '&' | '<' | '>' | '.' | '=' -> true
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
    | ']' :: _ -> true
    | '}' :: _ -> true
    | _ -> false

type QuoteLit  =
| LitQuote
| LitQuasiquote
| LitUnquote
| LitSplice

let quoteMap (c: char)  = 
    match c with
    | '\'' -> Some LitQuote
    | '~' -> Some LitQuasiquote
    | ',' -> Some LitUnquote
    | ';' -> Some LitSplice
    | _ -> None

let isQuoteChar (c: char) = (quoteMap c).IsSome

let quoteToSymbol (lit: QuoteLit option) (pos: PositionAnnotatedCode) =
    match lit with
    | Some LitQuote -> "quote"
    | Some LitQuasiquote -> "quasiquote"
    | Some LitUnquote -> "unquote"
    | Some LitSplice -> "splice"
    | None -> raise (failedAt "Tried to symbolize a non-quote char!" pos)


let handleStringEscapesSeqs (s: string) =
    s.Replace("\\n", "\n").Replace("\\r", "\r").Replace("\\t", "\t").Replace("\\\\", "\\")

let parseString(input: PositionAnnotatedCode) =
    let (str, rem) = splitOnChange<char> (fun c -> c <> '"') input.code
    let output = readN str.Length input
    let validStop =
        match rem with
        | '"' :: tail when isValidTermBorder(tail) -> true
        | _ -> false
    assertOrParseError output validStop "Unterminated string!"
    VickyValue.Str((listToString str) |> handleStringEscapesSeqs), (readN 1 output)


let parseNumber(input: PositionAnnotatedCode) =
    let (num, rest) = (splitOnChange isNum) input.code
    let output = readN (num.Length) input
    assertOrParseError output (isValidTermBorder rest) "Invalid characters at end of number!"
    let retVal = Num(num |> listToString |> Double.Parse)
    retVal, output

let parseComment(input: PositionAnnotatedCode) =
    let (comment, rest) = (splitOnChange (fun c -> c <> '\n')) input.code
    let output = readN (comment.Length) input
    let retVal = Str(comment |> listToString)
    retVal, output

let parseSymbol(input: PositionAnnotatedCode) =
    assertOrParseError input (isIdent input.code.Head) "Tried to call parseSymbol with non identifier starting character!"
    let (symbol, rest) = splitOnChange isIdent input.code 
    let output = readN symbol.Length input
    assertOrParseError output (isValidTermBorder rest) (sprintf "Invalid characters at end of symbol: %A" rest)
    let retVal = Symbol(symbol |> listToString)
    retVal, output

let parseKeyword(input: PositionAnnotatedCode) =
    assertOrParseError input (isIdent input.code.Head) "Tried to call parseSymbol with non identifier starting character!"
    let (symbol, rest) = splitOnChange isIdent input.code 
    let output = readN symbol.Length input
    assertOrParseError output (isValidTermBorder rest) (sprintf "Invalid characters at end of symbol: %A" rest)
    let retVal = Keyword(symbol |> listToString)
    retVal, output

let isArg (t: VickyValue) =
    match t with
    | VickyValue.Symbol(_) -> true
    | _ -> false


let argify (terms: VickyValue list) =
    seq {
        for t in terms do
            match t with
            | VickyValue.Symbol(s) -> s
            | _ -> raise (Exception (sprintf "Called argify incorrectly with %A!" terms))
    } |> List.ofSeq

let embody (terms: VickyValue list): VickyValue list list =
    seq {
        for t in terms do
            match t with
            | VickyValue.List(s) -> s
            | term -> [term]
            | _ -> raise (Exception (sprintf "Called embody incorrectly with %A!" terms))
    } |> List.ofSeq

let anonFnCounter = ref 0

let termIsSymbol (term: VickyValue) =
    match term with
    | VickyValue.Symbol(_) -> true
    | _ -> false

let termToSymbol (term: VickyValue) =
    match term with
    | VickyValue.Symbol(s) -> s
    | _ -> raise (ArgumentError (sprintf "Term isn't symbol! %A" term))

let genAnonName () = 
    anonFnCounter.Value <- anonFnCounter.Value + 1
    let ret = sprintf "ANON:[%A]" anonFnCounter.Value 
    ret

let rec parseTerms (baseInput: PositionAnnotatedCode): VickyValue * PositionAnnotatedCode =
    let mutable input = baseInput 
    // let mutable keepGoing = true 
    // let mutable results: VickyValue = Nil
    match input.code with
    //| c :: rest when isSpace c -> 
    //    input <- readN 1 input
    | '-' :: c :: _ when not (isNum c) -> Symbol("-"), (readN 1 input)
    | c :: _ when isNum c -> parseNumber input
    | ':' :: c :: _ when isIdent c -> parseKeyword (readN 1 input)
    | 'n' :: 'i' :: 'l' :: _ -> Nil, readN 3 input
    | '#' :: _ -> parseComment input
    | c :: _ when isIdent c -> parseSymbol input
    | q :: '(' :: _ when isQuoteChar q -> 
        let (value, rem) = parse (readN 2 input) (Some ')')
        List(Symbol(quoteToSymbol (quoteMap q) rem) :: [List(value)]), rem
    | q :: '[' :: _ when isQuoteChar q ->
        let (value, rem) = parse (readN 2 input) (Some ']')
        List(Symbol(quoteToSymbol (quoteMap q) rem) :: [Vector(value |> Array.ofList)]), rem
    | q :: '{' :: _ when isQuoteChar q ->
        let (value, rem) = parse (readN 2 input) (Some '}')
        if (value.Length % 2) <> 0 then
            raise (failedAt "Dict needs even number of elements" rem)
        let d = 
            List.chunkBySize 2 value 
            |> List.map (fun [a; b] -> a, b) 
            |> Map.ofList
            |> Dict
        (List(Symbol(quoteToSymbol (quoteMap q) rem) :: [d])), rem
    | q :: _ when isQuoteChar q ->
        let (value, rem) = parseTerms (readN 1 input)
        (List(Symbol(quoteToSymbol (quoteMap q) input) :: [value])), rem
    | '"' :: rest -> parseString (readN 1 input)
    | '(' :: rest -> 
        let (value, rem) = parse (readN 1 input) (Some ')')
        (VickyValue.List value), rem
    | '[' :: rest ->
        let (value, rem) = parse (readN 1 input) (Some ']')
        (Vector (value |> Array.ofList)), rem
    | '{' :: rest ->
        let (value, rem) = parse (readN 1 input) (Some '}')
        if (value.Length % 2) <> 0 then
            raise (failedAt "Dict needs even number of elements" input)
        let d = 
            List.chunkBySize 2 value 
            |> List.map (fun [a; b] -> a, b) 
            |> Map.ofList
            |> Dict
        d, rem
    | c -> raise (failedAt (sprintf "Unexpected character %A" c) input)

and parse (input: PositionAnnotatedCode) (endingDelim: char option) = 
    let mutable input = input
    let mutable results = []
    let mutable keepGoing = true

    while keepGoing && not input.code.IsEmpty do
        match input.code with
        | ' ' :: _ | '\t' :: _ | '\r' :: _ | '\n' :: _ -> 
            input <- readN 1 input
        | c :: _ when endingDelim.IsSome && endingDelim.Value = c -> 
            keepGoing <- false
            input <- readN 1 input
        | [] when endingDelim.IsSome ->
            raise (failedAt (sprintf "Missing delimiter %A" endingDelim.Value) input)
        | [] when endingDelim.IsNone -> 
            input <- input
            keepGoing <- false
        | c ->
            let (term, rem) = parseTerms input
            input <- rem
            results  <- term :: results
    List.rev results, input


let fnAdd (args: VickyValue list) (vm: VM) (env: Env) =
    let mutable sum = 0.0
    for a in args do
        match a with
        | VickyValue.Num n -> sum <- n + sum
        | v -> raise (exn "type-error" (sprintf "Unexpected value of type %A" v.GetType))
    VickyValue.Num(sum), env

let fnSubtract (args: VickyValue list) (vm: VM) (env: Env) =
    let mutable sum = 
        match args with
        | Num firstValue :: _ -> firstValue
        | head :: _ -> raise (exn "type-error" (sprintf "Cannot subtract starting with non-number %A" head))
        | _ -> raise (exn "type-error" ("Cannot subtract empty list"))

    for a in args.Tail do
        match a with
        | Num n -> sum <- sum - n
        | v -> raise (exn "type-error" (sprintf "Unexpected value of type %A" v.GetType))
    Num(sum), env

let fnDivide (args: VickyValue list) (vm: VM) (env: Env) =
    let mutable sum = 
        match args with
        | Num firstValue :: _ -> firstValue
        | head :: _ -> raise (exn "type-error" (sprintf "Cannot divide starting with non-number %A" head))
        | _ -> raise (exn "type-error" ("Cannot divide empty list"))

    for a in args.Tail do
        match a with
        | Num n -> sum <- sum / n
        | v -> raise (exn "type-error" (sprintf "Unexpected value of type %A" v.GetType))
    Num(sum), env

let fnMultiply (args: VickyValue list) (vm: VM) (env: Env) =
    let mutable sum = 1.0
    for a in args do
        match a with
        | Num n -> sum <- n * sum
        | v -> raise (exn "type-error" (sprintf "Unexpected value of type %A" v.GetType))
    Num(sum), env

let fnList (args: VickyValue list) (vm: VM) (env: Env) =
    List(args), env

let fnListPred (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | List(_) :: _ -> Boolean(true), env
    | _ -> Boolean(false), env

let fnEmptyPred (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | List(l) :: _ -> Boolean(l.IsEmpty), env
    | _ -> Boolean(false), env

let fnCount (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | List(l) :: _ -> Num(l.Length), env
    | Str(s) :: _ -> Num(s.Length), env
    | Symbol(s) :: _ -> Num(s.Length), env
    | Keyword(k) :: _ -> Num(k.Length), env
    | Vector(v) :: _ -> Num(v.Length), env
    | Dict(m) :: _ -> Num(m.Keys.Count), env
    | arg :: _ -> raise (exn "type-error" (sprintf "Cannot count value %A" arg))
    | _ -> raise (exn "type-error" "Cannot call 'count' without an argument")


let fnEq (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | a :: b :: [] ->  Boolean(a = b), env
    | _ -> raise (exn "type-error" (sprintf "Cannot call = with %A args" args.Length))

let fnLt (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | a :: b :: _ -> Boolean(a < b), env
    | _ -> raise (exn "type-error" (sprintf "Cannot call < with %A args" args.Length))

let fnLtEq (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | a :: b :: _ -> Boolean(a <= b), env
    | _ -> raise (exn "type-error" (sprintf "Cannot call <= with %A args" args.Length))

let fnGt (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | a :: b :: _ -> Boolean(a > b), env
    | _ -> raise (exn "type-error" (sprintf "Cannot call > with %A args" args.Length))

let fnGtEq (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | a :: b :: _ -> Boolean(a >= b), env
    | _ -> raise (exn "type-error" (sprintf "Cannot call >= with %A args" args.Length))

let fnAnd (args: VickyValue list) (vm: VM) (env: Env)  =
    let mutable argl = args
    let mutable keepGoing = true
    
    while not argl.Tail.IsEmpty && keepGoing do
        keepGoing <- not (isFalse argl.Head) && not (isNil argl.Head)
        if keepGoing then
            argl <- argl.Tail

    if not argl.IsEmpty then
        argl.Head, env
    else
        Nil, env

let fnOr (args: VickyValue list) (vm: VM) (env: Env)  =
    let mutable argl = args
    let mutable keepGoing = true
    
    while not argl.Tail.IsEmpty && keepGoing do
        keepGoing <- (isFalse argl.Head || isNil argl.Head)
        if keepGoing then
            argl <- argl.Tail

    if not argl.IsEmpty then
        argl.Head, env
    else
        Nil, env

let fnGet (args: VickyValue list) (vm: VM) (env: Env) = 
    match args with
    | Dict(m) :: key :: _ when m.ContainsKey(key) ->
        m[key], env
    | List(l) :: Num(idx) :: _ when idx < l.Length ->
        List.item (int idx) l, env
    | List(l) :: Num(idx) :: _ when idx > l.Length ->
        raise (exn "index-error" "Index too large for list!")
    | Vector(v) :: Num(idx) :: _ when idx < v.Length ->
        Array.item (int idx) v, env
    | Vector(v) :: Num(idx) :: _ when idx > v.Length ->
        raise (exn "index-error" "Index too large for vector!")
    | _ -> 
        Nil, env

let fnFirst (args: VickyValue list) (vm: VM) (env: Env) = 
    match args with
    | List(ret :: _) :: _ ->
        ret, env
    | Vector(v) :: _ when v.Length >= 1 ->
        v[0], env
    | _ -> 
        Nil, env
let fnRest (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | List(_ :: []) :: _ ->
        Nil, env
    | List(_ :: rest) :: _ ->
        List(rest), env
    | Vector(v) :: _ when v.Length >= 2 ->
        List((Array.sub v 1 (v.Length - 1)) |> List.ofArray), env
    | _ -> 
        Nil, env

let fnPut (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Dict(m) :: key :: value :: _ ->
        Dict(m.Add(key, value)), env
    | List(l) :: Num(idx) :: value :: _ when isInt idx && idx < l.Length ->
        List(List.updateAt (int idx) value l), env
    | List(l) :: Num(idx) :: _ when idx > l.Length ->
        raise (exn "index-error" "Index too large for list!")
    | Vector(v) :: Num(idx) :: value :: _ when idx < v.Length ->
        Vector(Array.updateAt (int idx) value v), env
    | Vector(v) :: Num(idx) :: _ when idx > v.Length ->
        raise (exn "index-error" "Index too large for vector!")
    | _ -> 
        Nil, env
        

let fnStrSplit(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | [Str(s); Str(delim)] -> 
        List(s.Split(delim) |> Seq.map (fun s -> Str(s)) |> List.ofSeq), env
    | [Str(s)] -> 
        List(s.Split(' ') |> Seq.map (fun s -> Str(s)) |> List.ofSeq), env
    | v -> raise (exn "type-error" (sprintf "Invalid arguments to str/split %s" (formatArgs args)))

let fnStr(args: VickyValue list) (vm: VM) (env: Env) =
    Str(String.Join("", (List.map 
    (fun v ->
        match v with 
        | Str(s) -> s
        | v -> formatValue v
    ) args))), env


let charMap = Map.ofList [
        ("ESC", '\x1B')
        ("NUL", '\x00')
        ("FILE-SEP", '\x1C')
        ("GROUP-SEP", '\x1D')
        ("RECORD-SEP", '\x1E')
        ("UNIT-SEP", '\x1F')
        ("FS", '\x1C')
        ("GS", '\x1D')
        ("RS", '\x1E')
        ("US", '\x1F')
        ("CR", '\r')
        ("LF", '\n')
    ]


let fnChars(args: VickyValue list) (vm: VM) (env: Env) =
    Str(String(Array.map 
        (fun v ->
            match v with 
            | Num(n) -> char n
            | Keyword(c) when c.Length = 1 -> c[0]
            | Keyword(key) when Map.containsKey key charMap -> charMap[key]
            | v -> raise (exn "type-error" (sprintf "Invalid arg passed to `chars`: %s" (formatValue v)))
        ) (Array.ofList args))) , env

let fnStrJoin(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(sep) :: List(parts) :: _ ->
        Str(String.Join(sep, (List.map 
        (fun v ->
            match v with 
            | Str(s) -> s
            | v -> formatValue v
        ) parts))), env
    | v -> raise (exn "type-error" (sprintf "Invalid arguments to str/join %s" (formatArgs args)))

let predStrHasPrefix (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(prefix) :: Str(str) :: [] -> Boolean (str.StartsWith(prefix)), env
    | v -> raise (exn "type-error" (sprintf "Invalid arguments to str/has-prefix %s" (formatArgs args)))

let predStrHasSuffix (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(suffix) :: Str(str) :: [] -> Boolean (str.EndsWith(suffix)), env
    | v -> raise (exn "type-error" (sprintf "Invalid arguments to str/has-prefix %s" (formatArgs args)))

let nativeFn name fn =
    Func(VickyFunc.Native(NativeFunc(name, fn)))

let nativeMacro name fn =
    Func(VickyFunc.Native(NativeFunc(name, fn, true)))

let rec resolveSymbol (symbol: Symbol) (env: Env): VickyValue option =
    if env.values.ContainsKey(VickyValue.Symbol(symbol)) then
        Some env.values[VickyValue.Symbol(symbol)]
    elif env.parent.IsNone then
        None
    else
        resolveSymbol symbol env.parent.Value

let rec setEnvValue (symbol: Symbol) (value: VickyValue) (env: Env): unit option =
    if env.values.ContainsKey(VickyValue.Symbol(symbol)) then
        env.values <- env.values.Add(Symbol(symbol), value)
        Some ()
    elif env.parent.IsNone then
        None
    else
        setEnvValue symbol value env.parent.Value

let formatStackFrame (s: StackFrame) =
    sprintf "%s:%s:%i" (s.GetFileName()) (s.GetMethod().Name) (s.GetFileLineNumber())

let resolveSymbolOrDie (symbol: Symbol) (env: Env): VickyValue =
    let resolved = resolveSymbol symbol env
    if resolved.IsNone then
        (* printf "" 
        let st = new StackTrace(true)
        let depthStr = new String('-', st.FrameCount)
        printf "%s" (String.Join("\r\n", (st.GetFrames() |> Seq.map formatStackFrame)))
        *)
        raise (exn "name-error" (sprintf "Couldn't find value for name %A" symbol))
    resolved.Value

let rec quasiquote (ast: VickyValue) =
    match ast with
    | List(Symbol("unquote") :: value :: _) ->
        value
    | List(l) ->
        let mutable result: VickyValue = Nil
        for elt in (List.rev l) do
            match elt with
            | List(Symbol("splice") :: second :: _) -> result <- List(Symbol("concat") :: second :: [result] )
            | el -> result <- List(Symbol("cons") :: quasiquote el :: [result])
        result
    | Symbol(_) as s -> List(Symbol("quote") :: [s])
    | Dict(_) as d -> List(Symbol("quote") :: [d])
    | term -> term

let isMacroValue (term: VickyValue option) =
    match term with
    | Some (Func(Defined(fn))) -> fn.isMacro
    | Some (Func(Native(fn))) -> fn.isMacro
    | _ -> false

let isMacroCall (env: Env) (term: VickyValue) =
    match term with
    | List(Symbol(s) :: args) -> isMacroValue (resolveSymbol s env)
    | _ -> false

let rec eval_ast (vm: VM) (term: VickyValue) =
    match term with
    | Symbol(s) -> resolveSymbolOrDie s vm.env
    | List(l) -> List(List.map (EVAL vm) l)
    | Vector(v) -> Vector(Array.map (EVAL vm) v)
    | Dict(d) -> Dict(Map.map (fun _ v -> (EVAL vm) v) d)
    | term -> term

and macroexpand (vm: VM) (term: VickyValue) = 
    let mutable resultingTerm = term
    while (isMacroCall vm.env resultingTerm) do
        match resultingTerm with
        | List(Symbol(s) :: args) as maybeMacro when isMacroCall vm.env maybeMacro ->
            resultingTerm <- apply vm (List((resolveSymbolOrDie s vm.env) :: args))
        | _ -> raise (exn "cannot-eval-error" (sprintf "Should Not Happen in Macroexpand! %s" (formatValue resultingTerm)))
    resultingTerm

and unspliceArgs (args: VickyValue list) (vm: VM) =
    // TODO: EVAL the splice argument here
    let mutable result: VickyValue list = []
    for elt in (List.rev args) do
        match elt with
        | List(Symbol("splice") :: ast :: _) ->
            match (EVAL vm ast) with
            | List(tosplice) ->
                result <- List.append result tosplice
            | Vector(tosplice) ->
                result <- List.append result (List.ofArray tosplice)
            | term ->
                result <- term :: result
        | term ->
            result <- term :: result
    result

and apply (vm: VM) (newList: VickyValue) =
    match newList with
    | VickyValue.List(VickyValue.Func(VickyFunc.Native(fn)) :: args) -> 
        let args = (unspliceArgs args vm)
        let (result, env) = fn.fn args vm vm.env
        result
    | VickyValue.List(VickyValue.Func(VickyFunc.Defined(fn)) :: args) -> 
        let oldEnv = vm.env
        vm.env <- Env(Map.empty, Some fn.defEnv)
        let minLen = min args.Length fn.args.Length
        let mutable currentArgName = fn.args
        let mutable currentArgValue = (unspliceArgs args vm)
        while not currentArgName.IsEmpty do
            match currentArgName with
            | "&" :: restArgName :: _ -> 
                vm.env <- addToEnv vm.env (VickyValue.Symbol restArgName) (List(currentArgValue))
                currentArgName <- []
                currentArgValue <- []
            | argName :: _ -> 
                vm.env <- addToEnv vm.env (VickyValue.Symbol argName) currentArgValue.Head
                currentArgValue <- currentArgValue.Tail
                currentArgName <- currentArgName.Tail
            | _ -> ()

        // TODO: Figure out how to deal with argument errors

        let mutable ret = Nil
        for term in fn.body do
            ret <- EVAL vm term
        vm.env <- oldEnv
        ret
    | VickyValue.List(head :: args) as term -> 
        raise (exn "cannot-eval-error" (sprintf "Cannot eval %A" term))
    | _ -> raise (exn "cannot-eval-error" "Should be impossible to reach this!")

and EVAL (vm: VM) (term: VickyValue)  =
    match (macroexpand vm term) with
    | List(l) when l.IsEmpty -> List([])
    | List(Symbol("def") :: Symbol(name) :: value :: _) ->
        let evaled = EVAL vm value
        vm.env.values <- vm.env.values.Add(Symbol(name), evaled)
        evaled
    | List(Symbol("let*") :: List(bindings) :: expr :: _) ->
        let mutable curEnv = Env(Map.empty, Some vm.env)
        vm.env <- curEnv
        let mutable ret = Nil
        for binding in (List.chunkBySize 2 bindings) do
            match binding with
            | [Symbol(name); term] -> 
                ret <- (EVAL vm term)
                vm.env <- addToEnv vm.env (Symbol name) ret    
            | _ -> raise (exn "name-error" (sprintf "Invalid binding %A" binding)) 
        let ret = EVAL vm expr
        vm.env <- vm.env.parent.Value
        ret

    | List(Symbol("if") :: cond :: true_form :: false_form :: _) ->
        match (EVAL vm cond) with
        | Boolean(false) | Nil -> EVAL vm false_form
        | _ -> EVAL vm true_form
    | List(Symbol("if") :: cond :: true_form ::  _) ->
        match (EVAL vm cond) with
        | Boolean(false) | Nil -> Nil
        | _ -> EVAL vm true_form
    | List(Symbol("fn") :: List(l) :: body ) ->
        Func(Defined(DefinedFunc((genAnonName ()), (List.map termToSymbol l), body, vm.env)))
    | List(Symbol("defmacro") :: Symbol(name) :: List(args) :: body) ->
        let mac = Func(Defined(DefinedFunc(name, (List.map termToSymbol args), body, vm.env, true)))
        vm.env.values <- vm.env.values.Add(Symbol name, mac)
        mac
    | List(Symbol("quote") :: arg :: _) ->
        arg
    | List(Symbol("splice") :: _) as form ->
        form
    | List(Symbol("quasiquote") :: ast :: _) ->
        let expanded = quasiquote ast
        EVAL vm expanded
    | List(Symbol("macroexpand") :: (List(_) as mac) :: _) ->
        macroexpand vm mac
    | List(Symbol("do") :: forms) ->
        let mutable ret = Nil
        for f in forms do
           ret <- EVAL vm f 
        ret
    | List(Symbol("while") :: cond :: body) ->
        let mutable ret = Nil
        while not (isFalse (EVAL vm cond)) do
            for form in body do
                ret <- EVAL vm form
        ret
    | List(_) as term -> 
        let newList = eval_ast vm term
        apply vm newList
    | term -> eval_ast vm term

let evalString(vm: VM) (code: string) (from: string) = 
    let (ast, _) = parse (posFromSource (code |> Seq.toList) from) None

    let result = List.map (EVAL vm) ast
    result

let canValueIter (value: VickyValue) =
    match value with
    | List(_) | Vector(_) | Dict(_) | Str(_) -> true
    | _ -> false

let valueIterOf (value: VickyValue) =
    match value with
    | List(l) -> Seq.ofList l 
    | Vector(v) -> Seq.ofArray v
    | Dict(m) -> m.Values
    | Str(s) -> Seq.map (fun c -> Num((double c))) (s.ToCharArray())
    | _ -> raise (exn "type-error" (sprintf "Cannot iterate values of %s" (formatValue value)))

let macEach (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Symbol(s) as varName :: maybeIter :: body -> 
        let maybeIter = (EVAL vm maybeIter)
        if not (canValueIter maybeIter) then
          raise (exn "type-error" (sprintf "Cannot loop `each` over %s" (formatValue (List args))))

        let iter = valueIterOf maybeIter
        let mutable ret = Nil
        for it in iter do
            let oldEnv = vm.env
            let vals = Map.empty<VickyValue, VickyValue>.Add(varName, it)
            vm.env <- Env(vals, (Some oldEnv))
            for b in body do
                ret <- EVAL vm b
            vm.env <- oldEnv
        ret, env
    | args -> raise (exn "type-error" (sprintf "Cannot loop `each` over %s" (formatValue (List args))))

let fnMap (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Func(Defined(_)) as toApply :: iter :: [] ->
        if canValueIter iter then
            List(List.map (fun el -> apply vm (List [toApply; el])) ((valueIterOf iter) |> List.ofSeq)), env
        else
            raise (exn "type-error" (sprintf "Cannot `map` over %s" (formatValue (List args))))
    | Func(Native(_)) as toApply :: iter :: [] ->
        if canValueIter iter then
            List(List.map (fun el -> apply vm (List [toApply; el])) ((valueIterOf iter) |> List.ofSeq)), env
        else
            raise (exn "type-error" (sprintf "Cannot `map` over %s" (formatValue (List args))))
    | args -> raise (exn "type-error" (sprintf "Cannot `map` over %s" (formatValue (List args))))


let fnSlurp (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(path) :: [] -> Str(File.ReadAllText(path)), env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to slurp %s" (formatArgs args)))

let fnSpit (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(path) :: Str(contents) :: [] -> 
        File.WriteAllText(path, contents)
        Nil, env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to slurp %s" (formatArgs args)))

let fnRead (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(input) :: _ -> 
        let (terms, _) = parse (posFromSource (input |> List.ofSeq) "read") None
        terms.Head, env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to slurp read-string %s" (formatArgs args)))
    
let fnEval (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | prog :: [] -> 
        EVAL vm prog, env 
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to eval %s" (formatArgs args)))

let fnAtom (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | value :: [] -> Atom(ref value), env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to atom %s" (formatArgs args)))

let fnAtomPred (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Atom(value) :: [] -> Boolean(true), env
    | _ -> Boolean(false), env

let fnDeref (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Atom(value) :: [] -> value.Value, env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to deref %s" (formatArgs args)))

let fnResetBang (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Atom(value) :: newValue :: _ -> 
        value.Value <- newValue
        newValue, env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to reset! %s" (formatArgs args)))

let fnSwapBang (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Atom(value) :: (VickyValue.Func(VickyFunc.Defined(fn)) as head) :: rest -> 
        apply vm (VickyValue.List(head :: value.Value :: rest)), env
    | Atom(value) :: (VickyValue.Func(VickyFunc.Native(fn)) as head) :: rest -> 
        apply vm (VickyValue.List(head :: value.Value :: rest)), env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to swap! %s" (formatArgs args)))

let fnCons (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | value :: List(l) :: _ -> List(value :: l), env
    | value :: Nil :: _ -> List([value]), env
    | _ -> raise (exn "type-error" (sprintf "Cannot cons using %s" (formatArgs args)))

let fnConcat (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | lists when List.forall isList lists -> 
        let toConcat = List.map asList lists
        VickyValue.List(List.concat (toConcat |> Seq.ofList)), env
    | _ -> raise (exn "type-error" (sprintf "Cannot cons using %s" (formatArgs args)))

let fnListRev (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | List(l) :: [] -> List(List.rev l), env
    | _ -> raise (exn "type-error" (sprintf "Cannot reverse using %s" (formatArgs args)))

let fnPrint (args: VickyValue list) (vm: VM) (env: Env) =
    for arg in args do
        match arg with
        | Str(s) -> printf "%s" s
        | v -> printf "%s" (formatValue v)
    printf "\n"
    Nil, env
let fnPrin (args: VickyValue list) (vm: VM) (env: Env) =
    for arg in args do
        match arg with
        | Str(s) -> printf "%s" s
        | v -> printf "%s" (formatValue v)
    Nil, env

let fnReadKey (args: VickyValue list) (vm: VM) (env: Env) =
    let keyInfo = Console.ReadKey(true)
    Str(String [|keyInfo.KeyChar|]), env

let fnCwd(args: VickyValue list) (vm: VM) (env: Env) =
    Str(Directory.GetCurrentDirectory()), env

let fnCd(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(path) :: _ -> 
        Directory.SetCurrentDirectory(path)
        Str(Directory.GetCurrentDirectory()), env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to os/cd %s" (formatArgs args)))

let fnOsGetEnv(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(key) :: _ -> 
        let osEnv = Environment.GetEnvironmentVariables()
        if osEnv.Contains(key) then
            Str(string(osEnv.Item key)), env
        else
            Nil, env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to os/getenv %A" (formatArgs args)))


let fnOsWhich (args: VickyValue list) (vm: VM) (env: Env) =
    if OperatingSystem.IsLinux() then
        Keyword("linux"), env
    elif OperatingSystem.IsWindows() then
        Keyword("windows"), env
    elif OperatingSystem.IsBrowser() then
        Keyword("web"), env
    elif OperatingSystem.IsMacOS() then
        Keyword("macos"), env
    else
        Keyword("unknown"), env
        
let fnOsPathExistsPred(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(path) :: _ -> Boolean(File.Exists(path) || Directory.Exists(path)), env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to os/path-exists? %A" (formatArgs args)))


let fnOsDir(args: VickyValue list) (vm: VM) (env: Env) = 
    match args with
    | Str(path) :: _ -> 
        List(
            List.map (fun s -> Str(s)) (Directory.EnumerateFileSystemEntries(path) |> List.ofSeq )), env
    | [] -> 
        List(
            List.map (fun s -> Str(s)) (Directory.EnumerateFileSystemEntries(".") |> List.ofSeq )), env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to os/dir %A" (formatArgs args)))

let fnFnBody(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Func(Defined fn) :: _ -> List(fn.body), env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to fn/body %A" (formatArgs args)))

let fnWithName(args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Symbol(name) :: Func(Defined fn) :: _ -> 
        fn.fullName <- name
        Func(Defined fn), env
    | _ -> raise (exn "type-error" (sprintf "Invalid arguments to fn/body %A" (formatArgs args)))

let fnCollectGarbage (args: VickyValue list) (vm: VM) (env: Env) =
    GC.Collect()
    Nil, env

let fnEnv(args: VickyValue list) (vm: VM) (env: Env) =
    Dict(env.values), env

let macSet (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Symbol(name) :: value :: _ when (resolveSymbol name env).IsSome ->
        let endVal = (EVAL vm value)
        ignore (setEnvValue name endVal env)
        endVal, env
    | Symbol(name) :: _ :: _ when (resolveSymbol name env).IsNone -> 
        raise(exn "type-error" (sprintf "Tried to set variable %s that doesn't exist!" name))
    | _ -> raise (exn "type-error" (sprintf "Invalid call to set with arguments: %A" (formatArgs args)))

let fnSet (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Symbol(name) :: value :: _ when (resolveSymbol name env).IsSome ->
        ignore (setEnvValue name value env)
        value, env
    | Symbol(name) :: _ :: _ when (resolveSymbol name env).IsNone -> 
        raise(exn "type-error" (sprintf "Tried to set variable %s that doesn't exist!" name))
    | _ -> raise (exn "type-error" (sprintf "Invalid call to set with arguments: %A" (formatArgs args)))

let macTry (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | attempted :: List(Symbol("catch") :: (Symbol(_) as errName) :: catchspec) :: [] ->
        try 
            EVAL vm attempted, env
        with
        | VickyExn(raised) -> 
            let oldEnv = vm.env
            let mutable ret = Nil
            let vals = Map.empty<VickyValue, VickyValue>.Add(errName, raised)
            vm.env <- Env(vals, (Some oldEnv))
            for b in catchspec do
                ret <- EVAL vm b
            vm.env <- oldEnv
            ret, env
    | _ -> raise (exn "form-error" (sprintf "Invalid body for `try` %s" (formatArgs args)))

let fnError (args: VickyValue list) (vm: VM) (env: Env) =
    match args with
    | Str(msg) :: [] -> raise (exn "error" msg)
    | data :: [] -> raise (exnDataOnly data)
    | Keyword(errType) :: Str(msg) :: [] -> raise (exn errType msg)
    | Keyword(errType) :: data :: [] -> raise (exnData errType "" data)
    | Keyword(errType) :: Str(msg) :: data ::[] -> raise (exnData errType msg data)
    | irregular -> raise (exnData "invalid-error-args" "\"error\" called with invalid arguments" (List irregular))

let defaultEnvValues: Map<VickyValue, VickyValue> = (Map.ofList [
    (Symbol "+", nativeFn "builtin::+" fnAdd)
    (Symbol "*", nativeFn "builtin::*" fnMultiply)
    (Symbol "-", nativeFn "builtin::-" fnSubtract)
    (Symbol "/", nativeFn "builtin::/" fnDivide)
    (Symbol "=", nativeFn "builtin::=" fnEq)
    (Symbol "<", nativeFn "builtin::<" fnLt)
    (Symbol ">", nativeFn "builtin::>" fnGt)
    (Symbol ">=", nativeFn "builtin::>=" fnGtEq)
    (Symbol "<=", nativeFn "builtin::<=" fnLtEq)
    (Symbol "or", nativeFn "builtin::or" fnOr)
    (Symbol "and", nativeFn "builtin::and" fnAnd)
    (Symbol "list", nativeFn "builtin::list" fnList)
    (Symbol "list?", nativeFn "builtin::list?" fnListPred)
    (Symbol "empty?", nativeFn "builtin::empty?" fnEmptyPred)
    (Symbol "count", nativeFn "builtin::count" fnCount)
    (Symbol "slurp", nativeFn "builtin::slurp" fnSlurp)
    (Symbol "spit", nativeFn "spit" fnSpit)
    (Symbol "read-str", nativeFn "builtin::read-str" fnRead)
    (Symbol "eval", nativeFn "builtin::eval" fnEval)
    (Symbol "get", nativeFn "builtin::get" fnGet)
    (Symbol "put", nativeFn "builtin::put" fnPut)
    (Symbol "set*", nativeFn "builtin::set*" fnSet)
    (Symbol "each", nativeMacro "builtin::each" macEach)
    (Symbol "try", nativeMacro "builtin::try" macTry)
    (Symbol "map", nativeFn "builtin::map" fnMap)
    (Symbol "error", nativeFn "builtin::error" fnError)
    (Symbol "first", nativeFn "builtin::first" fnFirst)
    (Symbol "rest", nativeFn "builtin::rest" fnRest)
    (Symbol "atom", nativeFn "builtin::atom" fnAtom)
    (Symbol "atom?", nativeFn "builtin::atom?" fnAtomPred)
    (Symbol "deref", nativeFn "builtin::deref" fnDeref)
    (Symbol "reset!", nativeFn "builtin::reset!" fnResetBang)
    (Symbol "swap!", nativeFn "builtin::swap!" fnSwapBang)
    (Symbol "cons", nativeFn "builtin::cons" fnCons)
    (Symbol "concat", nativeFn "builtin::concat" fnConcat)
    (Symbol "reverse", nativeFn "builtin::reverse" fnConcat)
    (Symbol "typeof", nativeFn "builtin::typeof" fnTypeof)
    (Symbol "str/split", nativeFn "str/split" fnStrSplit)
    (Symbol "str/join", nativeFn "str/join" fnStrJoin)
    (Symbol "str/has-prefix?", nativeFn "str/has-prefix?" predStrHasPrefix)
    (Symbol "str/has-suffix?", nativeFn "str/has-suffix?" predStrHasSuffix)
    (Symbol "str", nativeFn "str/str" fnStr)
    (Symbol "chars", nativeFn "str/chars" fnChars)
    (Symbol "os/cwd", nativeFn "os/cwd" fnCwd)
    (Symbol "os/cd", nativeFn "os/cd" fnCd)
    (Symbol "os/dir", nativeFn "os/dir" fnOsDir)
    (Symbol "os/getenv", nativeFn "os/getenv" fnOsGetEnv)
    (Symbol "os/which", nativeFn "os/which" fnOsWhich)
    (Symbol "os/path-exists?", nativeFn "os/path-exists?" fnOsPathExistsPred)
    (Symbol "print", nativeFn "builtin::print" fnPrint)
    (Symbol "prin", nativeFn "builtin::prin" fnPrin)
    (Symbol "read-key", nativeFn "builtin::read-key" fnReadKey)
    (Symbol "fn/body", nativeFn "fn/body" fnFnBody)
    (Symbol "fn/with-name", nativeFn "fn/with-name" fnWithName)
    (Symbol "gc/collect", nativeFn "gc/collect" fnCollectGarbage)

    (Symbol "true", Boolean(true))
    (Symbol "false", Boolean(false))
]) 

let defaultVm = 
    let env = Env (defaultEnvValues, None)
    let vm = VM(env)
    ignore (evalString vm """ (defmacro defn (name args & body) ~(def ,name (fn/with-name ',name (fn ,args ;body))))""" "boot")
    ignore (evalString vm "(defn not (a) (if a false true))" "boot")
    ignore (evalString vm "(defmacro set (name value) ~(set* ',name ,value))" "boot")
    ignore (evalString vm "(def nth get)" "boot") // Will need to change this later
    ignore (evalString vm """(defn load-file (f) (eval (read-string (str "(do " (slurp f) ")" ))))""" "boot") 
    ignore (evalString vm """
    (defmacro cond (& xs) 
            (if (> (count xs) 0) 
                (list 
                'if (first xs) 
                    (if (> (count xs) 1) 
                        (nth xs 1) 
                        (throw "odd number of forms to cond"))
                    (cons 'cond (rest (rest xs))))
                    ))
    """ "boot")


    ignore (evalString vm """
    (defn os/home-for-os (osName) 
        (cond 
            (= osName :windows) 
                (or (os/getenv "HOME") (os/getenv "USERPROFILE")) 
            true 
                (or (os/getenv "HOME") "")) )
    (defn os/home ()
        (let* (os-name (os/which))
            (os/home-for-os os-name)))

    (defn range (from to) 
        (if 
            (and 
                (= (typeof from) :number)
                (= (typeof to) :number)
                (< from to))
                (do
                    (def it to)
                    (def ret (list))
                    (while (<= from it)
                        (set ret (concat (list it) ret))
                        (set it (- it 1))
                    )
                    ret
                )
            (error (str "Cannot range " from " to " to "!"))))


    (defn esc (s) (str (chars :ESC) s))
    (defn black (s) (str (esc "[30m") s (esc "[0m")))
    (defn red (s) (str (esc "[31m") s (esc "[0m")))
    (defn green (s) (str (esc "[32m") s (esc "[0m")))
    (defn yellow (s) (str (esc "[33m") s (esc "[0m")))
    (defn blue (s) (str (esc "[34m") s (esc "[0m")))
    (defn magenta (s) (str (esc "[35m") s (esc "[0m")))
    (defn cyan (s) (str (esc "[36m") s (esc "[0m")))
    (defn white (s) (str (esc "[37m") s (esc "[0m")))

    (defn bg-black (s) (str (esc "[40m") s (esc "[0m")))
    (defn bg-red (s) (str (esc "[41m") s (esc "[0m")))
    (defn bg-green (s) (str (esc "[42m") s (esc "[0m")))
    (defn bg-yellow (s) (str (esc "[43m") s (esc "[0m")))
    (defn bg-blue (s) (str (esc "[44m") s (esc "[0m")))
    (defn bg-magenta (s) (str (esc "[45m") s (esc "[0m")))
    (defn bg-cyan (s) (str (esc "[46m") s (esc "[0m")))
    (defn bg-white (s) (str (esc "[47m") s (esc "[0m")))

    (defn bright-black (s) (str (esc "[30;1m") s (esc "[0m")))
    (defn bright-red (s) (str (esc "[31;1m") s (esc "[0m")))
    (defn bright-green (s) (str (esc "[32;1m") s (esc "[0m")))
    (defn bright-yellow (s) (str (esc "[33;1m") s (esc "[0m")))
    (defn bright-blue (s) (str (esc "[34;1m") s (esc "[0m")))
    (defn bright-magenta (s) (str (esc "[35;1m") s (esc "[0m")))
    (defn bright-cyan (s) (str (esc "[36;1m") s (esc "[0m")))
    (defn bright-white (s) (str (esc "[37;1m") s (esc "[0m")))

    (defn bg-bright-black (s) (str (esc "[40;1m") s (esc "[0m")))
    (defn bg-bright-red (s) (str (esc "[41;1m") s (esc "[0m")))
    (defn bg-bright-green (s) (str (esc "[42;1m") s (esc "[0m")))
    (defn bg-bright-yellow (s) (str (esc "[43;1m") s (esc "[0m")))
    (defn bg-bright-blue (s) (str (esc "[44;1m") s (esc "[0m")))
    (defn bg-bright-magenta (s) (str (esc "[45;1m") s (esc "[0m")))
    (defn bg-bright-cyan (s) (str (esc "[46;1m") s (esc "[0m")))
    (defn bg-bright-white (s) (str (esc "[47;1m") s (esc "[0m")))
    (defn cls () 
        (prin (esc "[0;0H"))
    )

    (defn move-cursor (x y) 
        (prin (esc (str "[" x ";" y "H")))
    )

    (def z-fg blue)
    (def z-bg bg-white)
    (def p-fg bright-yellow)
    (def p-bg bg-white)

    (def player-coord [10 10])
    (def old-coord player-coord)

    (defn line (x) 
        (str/join "" 
            (map 
                (fn (y) 
                    (if (= [x y] player-coord)
                        (str (p-bg (p-fg "@")))
                        (str (z-bg (z-fg "."))) 
                        ))
                (range 0 39) ) ))

    (defn block ()
        (str/join "\r\n"
            (map line (range 0 19))))

    (def going true)
    (defn v2add (a b) 
        [ (+ (get a 0) (get b 0))
          (+ (get a 1) (get b 1))])

    (defn handle-key (k) 
        (set old-coord player-coord)
        (cond 
            (= k "8") (set player-coord (v2add player-coord [-1 0]))
            (= k "2") (set player-coord (v2add player-coord [1 0]))
            (= k "6") (set player-coord (v2add player-coord [0 1]))
            (= k "4") (set player-coord (v2add player-coord [0 -1]))
            true (do)
        ))

    (defn game () 
        (prin (block))
        (while going 
            (handle-key (read-key))
            #(cls)
            #(prin (block))
            
            (move-cursor ;old-coord)
            (prin (str (z-bg (z-fg "."))))

            (move-cursor ;player-coord)
            (prin (str (p-bg (p-fg "@"))))
            )
    )

    (let* 
        (
        oldLoc (os/cwd)
        _ (os/cd (os/home))
        ret (if (os/path-exists? ".vicky_profile")
                (load-file ".vicky_profile"))
        ) 
        (do (os/cd oldLoc) ret))

    """ """boot""")

    vm
