namespace BookService.Common
open System

[<AutoOpen>]
module Common =
    let bind f = function
        | Ok s -> f s
        | Error e -> Error e

    let map f = function
        | Ok s -> Ok (f s)
        | Error e -> Error e

    type Result<'T, 'TError> with
        member x.Success 
            with get() = 
                match x with
                | Ok v -> v
                | _ -> failwith "Expected success result."

        member x.Error
            with get() =
                match x with
                | Error v -> v
                | _ -> failwith "Expected error result."

    type Errors =
    | InvalidArgument of string

[<AutoOpen>]
module NonEmptyString =

    type NonEmptyString = NonEmptyString of string

    let create s = 
        if not (String.IsNullOrWhiteSpace s)
            then Ok <| NonEmptyString s
        else Error (InvalidArgument "Found empty string when non empty was required.")

[<AutoOpen>]
module Id =
    type Id = Id of NonEmptyString

    let create s =
        NonEmptyString.create s |> bind (Id >> Ok)



