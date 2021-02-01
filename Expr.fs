module Expr

type Expr =
    | Int of int
    | IntList of int list
    | Var of string
    | SemiColon
    | Colon
    | Plus

let isValueExpr expr =
    match expr with
    | Int _ 
    | IntList _ 
    | Var _ -> true
    | SemiColon
    | Colon
    | Plus -> false

type Type =
    | IntType
    | IntListType

type Value =
    | IntValue of int
    | IntListValue of int list
