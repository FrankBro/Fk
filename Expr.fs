module Expr

type Expr =
    | Int of int
    | IntList of int list
    | Plus

let isValueExpr expr =
    match expr with
    | Int _ -> true
    | IntList _ -> true
    | Plus -> false
