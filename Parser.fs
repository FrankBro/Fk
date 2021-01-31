module Parser

open FParsec
open FParsec.CharParsers

open Expr

type Parser<'t> = Parser<'t, unit>

let sepBy2 p sep = p .>> sep .>>. sepBy1 p sep |>> List.Cons

// let nl = pchar '\n'
let ws = many (pchar ' ')
let ws1 = many1 (pchar ' ')

let str s = pstring s

let parseParenIntList =
    let value = pint32 .>> ws
    let sep = pchar ';' .>> ws
    between (pchar '(' >>. ws) (pchar ')') (sepBy2 value sep)
    |>> IntList

let parseSpaceIntList = sepBy2 pint32 ws1 |>> IntList

let parseIntList =
    attempt parseParenIntList
    <|> parseSpaceIntList

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
