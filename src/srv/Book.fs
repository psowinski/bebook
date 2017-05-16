namespace BookService.Domain
open System
open BookService.CommonTypes

module Book =
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

    //state -> command -> event
    let execute (state : Book) command =
        match command with
        | OpenBook x -> BookOpened x

    //state -> event -> state
    let apply (state: Book) event =
        match event with
        | BookOpened x -> x
