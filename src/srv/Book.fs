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

    type Event =
    | BookOpened of BookOpenRequest

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

        match command with
        | OpenBook x -> state |> 
                        validateOpening |> 
                        map (fun _ -> BookOpened x)

    let apply state event = //state -> event -> state
        let openBook bookReq = 
            { id = Some bookReq.id
              name = bookReq.name
              startDate = bookReq.startDate
              state = Open }

        match event with
        | BookOpened x -> x |> openBook |> Ok
