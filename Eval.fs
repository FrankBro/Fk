module Eval

open Expr

type Value =
    | IntValue of int

let evalPlus rhs exprs =
    match exprs with
    | [] ->
        failwith "flip"
    | lhs :: exprs when isValueExpr lhs ->
        match lhs, rhs with
        | IntExpr lhs, IntValue rhs ->
            IntValue (lhs + rhs), exprs
        | _ ->
            failwithf "Dyadic with %A and %A" lhs rhs
    | expr :: _ ->
        failwithf "Unexpected: %A" expr

let eval exprs = 
    let rec loop value exprs =
        match exprs with
        | [] -> value
        | expr :: exprs ->
            match value, expr with
            | Some _, expr when isValueExpr expr ->
                failwith "value but value is already set"
            | None, IntExpr i -> 
                loop (Some (IntValue i)) exprs
            | Some rhs, Plus ->
                let value, exprs = evalPlus rhs exprs
                loop (Some value) exprs
    exprs
    |> List.rev
    |> loop None
