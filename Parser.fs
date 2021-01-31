module Parser

open FParsec
open FParsec.CharParsers

open Expr

type Parser<'t> = Parser<'t, unit>

// let nl = pchar '\n'
let ws = many (pchar ' ')

let str s = pstring s

let parseIntExpr = pint32 |>> IntExpr

let parseExpr : Parser<Expr> = choice [
    str "+" >>% Plus
    parseIntExpr
]

let readOrThrow parser input =
    match run parser input with
    | ParserResult.Success (result, state, pos) -> result
    | ParserResult.Failure (se, e, state) -> failwith se
let readLine input =
    let parser = ws >>. sepEndBy parseExpr ws
    readOrThrow parser input
