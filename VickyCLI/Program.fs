open Interpreter
open Tests
open System

let repl(vm: VM) =
    let mutable exit = false
    let mutable evalLine = 0
    vm.env <- vm.env.Add(
        "exit", 
        nativeFn "repl::exit" (fun _ _ env  -> exit <- true; VickyValue.Null, env))

    while not exit do
        try
           printf "user> "
           let result = evalString vm (Console.ReadLine()) (sprintf "repl:%A" evalLine)
           printfn "%s" (formatValue result)
           evalLine <- evalLine + 1
        with
        | ParseFailed({ 
            message = m; 
            line = l;
            column = c;
            from = source;
        }) -> (printfn "Parse failure in %A:%A:%A:\n\t%A" source l c m)
        | CannotEvalError(m) -> (printfn "%A" m)
        | InvalidNameException(m) -> (printfn "%A" m)

[<EntryPoint>]
let main(args) =
    let vm = VM(defaultEnv, [])
    test vm
    printfn ""
    repl vm
    0
