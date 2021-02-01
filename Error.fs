module Error

open Expr

type FkError =
    | UnexpectedLhsType of lhs: Expr * op: Expr * rhs: Type
    | UnexpectedLhsValue of lhs: Expr * op: Expr * rhs: Value
    | UndefinedVar of string
    | WrongType of expected: Type * actual: Type
    | WrongValue of expected: Type * actual: Value
    | LengthMismatch of lhs: Expr * op: Expr * rhs: Value

exception FkException of FkError
