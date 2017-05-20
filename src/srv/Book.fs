namespace BookService.Domain
open System
open BookService.Common

module Book =

    type BookError =
    | InvalidBookId
    | InvalidBookName

    type Book = {
        id: Id option
        name: string
        startDate: DateTime
    }

    type Command =
    | OpenBook of Book

    type Event =
    | BookOpened of Book

    let zero =
        { id = None; name = null; startDate = DateTime.Now }

    let validateId book =
        match book.id with
        | Some _ -> Ok book
        | None -> Error InvalidBookId

    let validateName book =
        if String.IsNullOrWhiteSpace book.name
            then Error InvalidBookName
        else Ok book

    let validateBook = validateId >> (bind validateName)

    let execute state command = //state -> command -> event
        match command with
        | OpenBook x -> x |> validateBook |> bind (BookOpened >> Ok)

    let apply state event = //state -> event -> state
        match event with
        | BookOpened x -> x
