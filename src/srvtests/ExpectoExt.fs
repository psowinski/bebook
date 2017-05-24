module ShouldBe

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

let error x message =
    match x with
    | Error _ -> ()
    | Ok v -> Expecto.Tests.failtestf "%s. Expected Error, was Ok (%A)." message v

let ok x message =
    match x with
    | Ok _ -> ()
    | Error e ->  Expecto.Tests.failtestf "%s. Expected Ok, was Error (%A)." message e

let errorOf case x message =
    match x with
    | Error e when (isUnionCase case e) -> ()
    | Error _
    | Ok _ -> Expecto.Tests.failtestf "%s. Expected Error of %A, was %A." message case x

let okOf case x message =
    match x with
    | Ok s when (isUnionCase case s) -> ()
    | Ok _
    | Error _ -> Expecto.Tests.failtestf "%s. Expected Ok of %A, was %A." message case x
