module Expr

type Expr =
    | IntExpr of int
    // | IntValues of int[]
    | Plus

let isValueExpr expr =
    match expr with
    | IntExpr _ -> true
    // | IntValues _ -> true
    | Plus -> false
