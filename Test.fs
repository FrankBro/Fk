module Test

open Xunit

open Eval
open Expr
open Infer
open TestUtil

[<Fact>]
let ``An integer`` () =
    testLine
        "3"
        (POk [[Int 3]])
        (IOk (Some IntType))
        (EOk (Some (IntValue 3)))

[<Fact>]
let ``Add two integers`` () =
    testLine
        "3+4"
        (POk [[Int 3; Plus; Int 4]])
        (IOk (Some IntType))
        (EOk (Some (IntValue 7)))

[<Fact>]
let ``An integer list`` () =
    testLine
        "1 2 3"
        (POk [[IntList [1;2;3]]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [1;2;3])))

[<Fact>]
let ``An integer list with parens`` () =
    testLine
        "( 1 ;2; 3 )"
        (POk [[IntList [1;2;3]]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [1;2;3])))

[<Fact>]
let ``Add an integer to an integer list`` () =
    testLine
        "3+1 2 3"
        (POk [[Int 3; Plus; IntList [1;2;3]]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [4;5;6])))

[<Fact>]
let ``Add an integer list to an integer`` () =
    testLine
        "1 2 3+3"
        (POk [[IntList [1;2;3]; Plus; Int 3]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [4;5;6])))

[<Fact>]
let ``Semicolon`` () =
    testLine
        "3;4"
        (POk [[Int 3]; [Int 4]] )
        (IOk (Some IntType))
        (EOk (Some (IntValue 4)))

[<Fact>]
let ``Int var set and get`` () =
    testLine
        "a:3;a"
        (POk [[Var "a"; Colon; Int 3]; [Var "a"]])
        (IOk (Some IntType))
        (EOk (Some (IntValue 3)))

[<Fact>]
let ``Int var plus int`` () =
    testLine
        "a:3;a+4"
        (POk [[Var "a"; Colon; Int 3]; [Var "a"; Plus; Int 4]])
        (IOk (Some IntType))
        (EOk (Some (IntValue 7)))

[<Fact>]
let ``Int plus int var`` () =
    testLine
        "a:3;4+a"
        (POk [[Var "a"; Colon; Int 3]; [Int 4; Plus; Var "a"]])
        (IOk (Some IntType))
        (EOk (Some (IntValue 7)))

[<Fact>]
let ``Int var plus int list`` () =
    testLine
        "a:3;a+4 5"
        (POk [[Var "a"; Colon; Int 3]; [Var "a"; Plus; IntList [4; 5]]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [7; 8])))

[<Fact>]
let ``Int list plus int var`` () =
    testLine
        "a:3;4 5+a"
        (POk [[Var "a"; Colon; Int 3]; [IntList  [4; 5]; Plus; Var "a"]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [7; 8])))

[<Fact>]
let ``Int list var set and get`` () =
    testLine
        "a:3 4 5;a"
        (POk [[Var "a"; Colon; IntList [3; 4; 5]]; [Var "a"]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [3; 4; 5])))

[<Fact>]
let ``Int list var plus int`` () =
    testLine
        "a:3 4;a+4"
        (POk [[Var "a"; Colon; IntList [3; 4]]; [Var "a"; Plus; Int 4]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [7; 8])))

[<Fact>]
let ``Int plus int list var`` () =
    testLine
        "a:3 4;4+a"
        (POk [[Var "a"; Colon; IntList [3; 4]]; [Int 4; Plus; Var "a"]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [7; 8])))
