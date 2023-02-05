open Interpreter
// open Tests
open System

let repl(vm: VM) =
    let start = DateTime.Now.Ticks
    let mutable exit = false
    let mutable evalLine = 0
    vm.env.values <- vm.env.values.Add(
        Symbol "exit", 
        nativeFn "repl::exit" (fun _ _ env  -> exit <- true; List([]), env))
  //  vm.env.values <- vm.env.values.Add( Symbol "tests/run",
  //      nativeFn "tests/run" (fun _ vm _ -> test vm; Nil, vm.env))
    let endTime = DateTime.Now.Ticks
    printfn "REPL startup time: %A" (endTime - start)

    while not exit do
        try
           printf "user> "
           let result = evalString vm (Console.ReadLine()) (sprintf "repl:%A" evalLine) 

           for s in result  do
               printf "%s " (formatValue s)
           printfn ""
           evalLine <- evalLine + 1
        with
        | ParseFailed({ 
            message = m; 
            line = l;
            column = c;
            from = source;
        }) -> (printfn "Parse failure in %s:%A:%A:\n\t%s" source l c m)
        | CannotEvalError(m) -> (printfn "%s" m)
        | InvalidNameException(m) -> (printfn "%s" m)
        | ArgumentError(m) -> (printfn "%s" m)
        | TypeError(m) -> (printfn "%s" m)
        | e -> (printfn "Interpreter Coding error: %s %s" (e.GetType().Name) e.Message)

[<EntryPoint>]
let main(args) =
    let start = DateTime.Now.Ticks
    let vm = defaultVm
    let endTime = DateTime.Now.Ticks
    printf "startup time: %A\n" (endTime - start)
    repl vm
    0
