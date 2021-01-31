open System

// Define a function to construct a message to print
[<EntryPoint>]
let main argv =
    let input = "3+3"
    let parsed = Parser.readLine input
    printfn "%A" parsed
    let evaled = Eval.eval parsed
    printfn "%A" evaled
    0 // return an integer exit code
