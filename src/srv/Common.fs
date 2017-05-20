namespace BookService.Common
open System

[<AutoOpen>]
module Common =

    type NonEmptyString = NonEmptyString of string

    type Id = Id of string

    let bind f x =
        match x with
        | Ok v -> f v
        | Error v -> Error v
