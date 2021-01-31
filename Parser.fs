module Parser

open FParsec
open FParsec.CharParsers

open Expr

type Parser<'t> = Parser<'t, unit>

// let nl = pchar '\n'
let ws = many (pchar ' ')
let ws1 = many1 (pchar ' ')

let str s = pstring s

let parseIntList = pint32 .>> ws1 .>>. sepBy1 pint32 ws1 |>> fun (x, xs) -> IntList (x :: xs)

let parseInt = pint32 |>> Int

let parseExpr : Parser<Expr> = choice [
    str "+" >>% Plus
    attempt parseIntList
    parseInt
]

let readOrThrow parser input =
    match run parser input with
    | ParserResult.Success (result, state, pos) -> result
    | ParserResult.Failure (se, e, state) -> failwith se
let readLine input =
    let parser = ws >>. sepEndBy parseExpr ws
    readOrThrow parser input
