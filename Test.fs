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
        (POk [Int 3])
        (IOk (Some IntType))
        (EOk (Some (IntValue 3)))

[<Fact>]
let ``Add two integers`` () =
    testLine
        "3+4"
        (POk [Int 3; Plus; Int 4])
        (IOk (Some IntType))
        (EOk (Some (IntValue 7)))

[<Fact>]
let ``An integer list`` () =
    testLine
        "1 2 3"
        (POk [IntList [1;2;3]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [1;2;3])))

[<Fact>]
let ``An integer list with parens`` () =
    testLine
        "( 1 ;2; 3 )"
        (POk [IntList [1;2;3]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [1;2;3])))

[<Fact>]
let ``Add an integer to an integer list`` () =
    testLine
        "3+1 2 3"
        (POk [Int 3; Plus; IntList [1;2;3]])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [4;5;6])))

[<Fact>]
let ``Add an integer list to an integer`` () =
    testLine
        "1 2 3+3"
        (POk [IntList [1;2;3]; Plus; Int 3])
        (IOk (Some IntListType))
        (EOk (Some (IntListValue [4;5;6])))
