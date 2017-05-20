module BookService.EdgeServices
open BookService.Common
open System

let createNonEmptyString s =
    if String.IsNullOrWhiteSpace s
        then Ok <| NonEmptyString s
    else Error "Empty string"
