module Infer

open Error
open Expr


let inferColon (env: Map<string, Type>) rhs exprs =
    match exprs with
    | [] ->
        failwith ":x"
    | Var name :: exprs ->
        let env = Map.add name rhs env
        env, exprs
    | lhs :: _ ->
        raise (FkException (UnexpectedLhsType (lhs, Colon, rhs)))

let inferPlus env rhs exprs =
    match exprs with
    | [] ->
        failwith "flip"
    | lhs :: exprs when isValueExpr lhs ->
        match lhs, rhs with
        | Var name, IntType ->
            match Map.tryFind name env with
            | None ->
                raise (FkException (UndefinedVar name))
            | Some IntType ->
                IntType, exprs
            | Some IntListType ->
                IntListType, exprs
            | Some lhs ->
                raise (FkException (WrongType (IntType, lhs)))
        | Var name, IntListType ->
            match Map.tryFind name env with
            | None ->
                raise (FkException (UndefinedVar name))
            | Some IntType ->
                IntListType, exprs
            | Some lhs ->
                raise (FkException (WrongType (IntType, lhs)))
        | Int _, IntType ->
            IntType, exprs
        | Int _, IntListType ->
            IntListType, exprs
        | IntList _, IntType ->
            IntListType, exprs
        | IntList _, IntListType ->
            IntListType, exprs
        | _ ->
            failwithf "Dyadic with %A and %A" lhs rhs
    | expr :: _ ->
        failwithf "Unexpected: %A" expr

let inferLine env exprs =
    let rec loop env rhs exprs =
        match exprs with
        | [] -> env, rhs
        | expr :: exprs ->
            match rhs, expr with
            | Some _, expr when isValueExpr expr ->
                failwith "typ but typ is already set"
            | None, IntList _ ->
                loop env (Some IntListType) exprs
            | None, Int _ ->
                loop env (Some IntType) exprs
            | None, Var name ->
                match Map.tryFind name env with
                | None ->
                    raise (FkException (UndefinedVar name))
                | Some rhs ->
                    loop env (Some rhs) exprs
            | Some rhs, Colon ->
                let env, exprs = inferColon env rhs exprs
                loop env None exprs
            | Some rhs, Plus ->
                let typ, exprs = inferPlus env rhs exprs
                loop env (Some typ) exprs
            | _ ->
                failwithf "match incomplete: rhs = %A and expr = %A" rhs expr
    exprs
    |> List.rev
    |> loop env None

let infer lines =
    let rec loop env typ lines =
        match lines with
        | [] -> typ
        | line :: lines ->
            let env, typ = inferLine env line
            loop env typ lines
    lines
    |> loop Map.empty None
