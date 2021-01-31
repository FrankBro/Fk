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
        (POk [IntExpr 3])
        (IOk (Some IntType))
        (EOk (Some (IntValue 3)))

[<Fact>]
let ``Add two integers`` () =
    testLine
        "3+4"
        (POk [IntExpr 3; Plus; IntExpr 4])
        (IOk (Some IntType))
        (EOk (Some (IntValue 7)))
