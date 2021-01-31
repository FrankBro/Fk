module Infer

open Expr

type Type =
    | IntType

let inferPlus rhs exprs =
    match exprs with
    | [] ->
        failwith "flip"
    | lhs :: exprs when isValueExpr lhs ->
        match lhs, rhs with
        | IntExpr lhs, IntType ->
            IntType, exprs
        | _ ->
            failwithf "Dyadic with %A and %A" lhs rhs
    | expr :: _ ->
        failwithf "Unexpected: %A" expr


let infer exprs =
    let rec loop typ exprs =
        match exprs with
        | [] -> typ
        | expr :: exprs ->
            match typ, expr with
            | Some _, expr when isValueExpr expr ->
                failwith "typ but typ is already set"
            | None, IntExpr _ ->
                loop (Some IntType) exprs
            | Some rhs, Plus ->
                let typ, exprs = inferPlus rhs exprs
                loop (Some typ) exprs
    exprs
    |> List.rev
    |> loop None