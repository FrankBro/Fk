module Eval

open Expr

type Value =
    | IntValue of int
    | IntListValue of int list

let evalPlus rhs exprs =
    match exprs with
    | [] ->
        failwith "flip"
    | lhs :: exprs when isValueExpr lhs ->
        match lhs, rhs with
        | Int lhs, IntValue rhs ->
            IntValue (lhs + rhs), exprs
        | Int lhs, IntListValue rhs ->
            IntListValue (List.map (fun rhs -> lhs + rhs) rhs), exprs
        | IntList lhs, IntValue rhs ->
            IntListValue (List.map (fun lhs -> lhs + rhs) lhs), exprs
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
            | None, Int i -> 
                loop (Some (IntValue i)) exprs
            | None, IntList is ->
                loop (Some (IntListValue is)) exprs
            | Some rhs, Plus ->
                let value, exprs = evalPlus rhs exprs
                loop (Some value) exprs
    exprs
    |> List.rev
    |> loop None
