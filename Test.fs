module Test

open Xunit

open Eval
open Expr
open Infer
open TestUtil

[<Fact>]
let ``Integer`` () =
    testLine
        "3"
        (POk [IntExpr 3])
        (IOk (Some IntType))
        (EOk (Some (IntValue 3)))
