open Interpreter
open Tests
open System

let repl(vm: VM) =
    let mutable exit = false
    while not exit do
        try
           printf "user> "
           let result = evalString vm (Console.ReadLine())
           printfn "%A" result
        with
        | ParseFailed({ message = m }) -> (printfn "Failed to parse:\n\t%A" m)
        | CannotExecuteException(m) -> (printfn "Could not execute term:\n\t%A" m)
        | InvalidNameException(m) -> (printfn "%A" m)

[<EntryPoint>]
let main(args) =
    let vm = VM(defaultEnv, [])
    test vm
    printfn ""
    repl vm
    0
