module Tests
open Interpreter

exception TestFailed of string

let check expr expected message =
    match expr with
    | e when e = expected -> printf "."
    | _ -> raise (TestFailed message)

let testTopLevel (vm: VM) =
    check (evalString vm "12") (VickyValue.Num(12.0)) "Toplevel numbers don't work"
    check (evalString vm "true") (VickyValue.Boolean(true)) "Toplevel bools don't work"
    check (evalString vm "(list 1 2 3)") (VickyValue.List([
        VickyValue.Num(1.0);
        VickyValue.Num(2.0);
        VickyValue.Num(3.0);
    ])) "Toplevel lists don't work"

let testAddition (vm: VM) =
    let result = evalString vm "(+ 1 2)"
    check result (VickyValue.Num(3.0)) "(+ 1 2) wasn't 3"
    let result = evalString vm "(+ 1 2 3)"
    check result (VickyValue.Num(6.0)) "(+ 1 2 3) wasn't 6"


let test (vm: VM) =
    testTopLevel vm
    testAddition vm
