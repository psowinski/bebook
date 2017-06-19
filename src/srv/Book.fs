namespace BookService.Domain
open System
open BookService.Common

module Book =

    type BookState =
    | NotOpenYet
    | Open
    | Closed

    type Book = {
        id: Id option
        name: NonEmptyString
        startDate: DateTime
        state: BookState }
    
    type BookOpenRequest = {
        id: Id
        name: NonEmptyString
        startDate: DateTime }

    type Command =
    | OpenBook of BookOpenRequest
    | CloseBook

    type Event =
    | BookOpened of BookOpenRequest
    | BookClosed

    let zero =
        { id = None
          name = NonEmptyString "..."
          startDate = DateTime.Now
          state = NotOpenYet }
    let execute state command = //state -> command -> event

        let validateOpening book = 
            match book.state with
            | NotOpenYet -> Ok book
            | _ -> Error <| InvalidArgument "Book.state shoud be NotOpenYet"

        let validateClosing book = 
            match book.state with
            | Open -> Ok book
            | _ -> Error <| InvalidArgument "Book.state shoud be Open"

        match command with
        | OpenBook x -> state |> validateOpening |> 
                        map (fun _ -> BookOpened x)
        | CloseBook -> state |> validateClosing |>
                        map (fun _ -> BookClosed)

    let apply state event = //state -> event -> state
        let openBook bookReq = 
            { id = Some bookReq.id
              name = bookReq.name
              startDate = bookReq.startDate
              state = Open }

        match event with
        | BookOpened x -> x |> openBook |> Ok
        | BookClosed -> { state with state = Closed } |> Ok
