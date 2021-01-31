module Error

type FkError =
    | Todo

exception FkException of FkError
