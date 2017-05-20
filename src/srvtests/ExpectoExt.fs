module Should.Expect

open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection

let rec isUnionCase = function
    | Lambda (_, expr)
    | Let (_, _, expr) -> isUnionCase expr
    | NewTuple exprs -> 
        let iucs = List.map isUnionCase exprs
        fun value -> List.exists ((|>) value) iucs
    | NewUnionCase (uci, _) ->
        let utr = FSharpValue.PreComputeUnionTagReader uci.DeclaringType
        box >> utr >> (=) uci.Tag
    | _ -> failwith "Expression is no union case."

let isError x message =
    match x with
    | Error _ -> ()
    | Ok v -> Expecto.Tests.failtestf "%s. Expected Error, was Ok (%A)." message v

let isErrorOf case x message =
    match x with
    | Error v when (isUnionCase case v) -> ()
    | Error _
    | Ok _ -> Expecto.Tests.failtestf "%s. Expected Error of %A, was %A." message case x
