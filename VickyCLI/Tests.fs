module Tests
open Interpreter

exception TestFailed of string

let check expr expected message =
    match expr with
    | e when e = expected -> printf "."
    | _ -> raise (TestFailed message)

let testTopLevel (vm: VM) =
    check (evalString vm "12" "lit:testTopLevel 1") (VickyValue.Num(12.0)) "Toplevel numbers don't work"
    check (evalString vm "true" "lit:testTopLevel 2") (VickyValue.Boolean(true)) "Toplevel bools don't work"
    check (evalString vm "(list 1 2 3)" "lit:testTopLevel 3") (VickyValue.List([
        VickyValue.Num(1.0);
        VickyValue.Num(2.0);
        VickyValue.Num(3.0);
    ])) "Toplevel lists don't work"

let testAddition (vm: VM) =
    let result = evalString vm "(+ 1 2)" "lit:testAddition 1"
    check result (VickyValue.Num(3.0)) "(+ 1 2) wasn't 3"
    let result = evalString vm "(+ 1 2 3)" "list:testAddition 2"
    check result (VickyValue.Num(6.0)) "(+ 1 2 3) wasn't 6"

let testDefs (vm: VM) =
    check (evalString vm "(def a 1)" "lit:testDefs 1") (VickyValue.Num(1.0)) "Top level def doesn't work"
    check (evalString vm "(def b a)" "lit:testDefs 2") (VickyValue.Num(1.0)) "Def sequencing doesn't work"

let testQuotes (vm: VM) =
    check (evalString vm "'(1 2)" "lit:testQuotes 1") (VickyValue.List([
        VickyValue.Num(1.0); 
        VickyValue.Num(2.0)
    ])) "Quoting lists doesn't work"
    check (evalString vm "'1" "lit:testQuotes 2") (VickyValue.Num(1.0)) "Quoting numbers doesn't work"
    check (evalString vm "'str/split" "lit:testQuotes 3") (VickyValue.Symbol("str/split")) "Quoting symbols doesn't work"


let test (vm: VM) =
    testTopLevel vm
    testAddition vm
    testDefs vm
    testQuotes vm
