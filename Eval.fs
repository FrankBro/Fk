module Eval

open Error
open Expr

let evalColon env rhs exprs =
    match exprs with
    | [] ->
        failwith ":x"
    | Var name :: exprs ->
        let env = Map.add name rhs env
        env, exprs
    | lhs :: _ ->
        raise (FkException (UnexpectedLhsValue (lhs, Colon, rhs)))

let evalPlus env rhs exprs =
    match exprs with
    | [] ->
        failwith "flip"
    | lhs :: exprs when isValueExpr lhs ->
        match lhs, rhs with
        | Var name, IntValue rhs ->
            match Map.tryFind name env with
            | None ->
                raise (FkException (UndefinedVar name))
            | Some (IntValue lhs) ->
                IntValue (lhs + rhs), exprs
            | Some (IntListValue lhs) ->
                IntListValue (List.map (fun lhs -> lhs + rhs) lhs), exprs
            | Some lhs ->
                raise (FkException (WrongValue (IntType, lhs)))
        | Var name, IntListValue rhs ->
            match Map.tryFind name env with
            | None ->
                raise (FkException (UndefinedVar name))
            | Some (IntValue lhs) ->
                IntListValue (List.map (fun rhs -> lhs + rhs) rhs), exprs
            | Some lhs ->
                raise (FkException (WrongValue (IntType, lhs)))
        | Int lhs, IntValue rhs ->
            IntValue (lhs + rhs), exprs
        | Int lhs, IntListValue rhs ->
            IntListValue (List.map (fun rhs -> lhs + rhs) rhs), exprs
        | IntList lhs, IntValue rhs ->
            IntListValue (List.map (fun lhs -> lhs + rhs) lhs), exprs
        | IntList lhs, IntListValue rhs ->
            if List.length lhs <> List.length rhs then
                raise (FkException (LengthMismatch (IntList lhs, Plus, IntListValue rhs)))
            else
                let value =
                    List.zip lhs rhs
                    |> List.map (fun (lhs, rhs) ->
                        lhs + rhs
                    )
                    |> IntListValue
                value, exprs
        | _ ->
            failwithf "Dyadic with %A and %A" lhs rhs
    | expr :: _ ->
        failwithf "Unexpected: %A" expr

let evalLine env exprs = 
    let rec loop env rhs exprs =
        match exprs with
        | [] -> env, rhs
        | expr :: exprs ->
            match rhs, expr with
            | Some _, expr when isValueExpr expr ->
                failwith "value but value is already set"
            | None, Int i -> 
                loop env (Some (IntValue i)) exprs
            | None, IntList is ->
                loop env (Some (IntListValue is)) exprs
            | None, Var name ->
                match Map.tryFind name env with
                | None ->
                    raise (FkException (UndefinedVar name))
                | Some rhs ->
                    loop env (Some rhs) exprs
            | Some rhs, Colon ->
                let env, exprs = evalColon env rhs exprs
                loop env None exprs
            | Some rhs, Plus ->
                let value, exprs = evalPlus env rhs exprs
                loop env (Some value) exprs
    exprs
    |> List.rev
    |> loop env None

let eval lines =
    let rec loop env value lines =
        match lines with
        | [] -> value
        | line :: lines ->
            let env, value = evalLine env line
            loop env value lines
    lines
    |> loop Map.empty None
