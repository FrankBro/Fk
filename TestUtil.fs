module TestUtil

open Xunit

open Error
open Eval
open Expr
open Infer
open Parser

type ParserResult =
    | POk of Expr list
    | PSkip
    | PFail of FkError

type InferResult =
    | IOk of Type option
    | ISkip
    | IFail of FkError

type EvalResult =
    | EOk of Value option
    | ESkip
    | EFail of FkError

let testLine input parserExpected inferExpected evalExpected =
    let parserActual =
        try
            readLine input
            |> POk
        with
        | FkException error -> PFail error
    if parserExpected <> PSkip then
        Assert.StrictEqual (parserExpected, parserActual)
    let inferActual =
        match parserActual with
        | PSkip -> failwith "impossible"
        | PFail e -> IFail e
        | POk exprs ->
            try
                infer exprs
                |> IOk
            with
            | FkException error -> IFail error
    if inferExpected <> ISkip then
        Assert.StrictEqual (inferExpected, inferActual)
    let evalActual =
        match parserActual with
        | PSkip -> failwith "impossible"
        | PFail e -> EFail e
        | POk exprs ->
            try
                eval exprs
                |> EOk
            with
            | FkException error -> EFail error
    if evalExpected <> ESkip then
        Assert.StrictEqual (evalExpected, evalActual)


