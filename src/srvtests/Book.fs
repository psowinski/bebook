module Tests

open Expecto
open BookService.Common
open BookService.Domain
open Book
open System

[<Tests>]
let bookTests =
  let bookId = (Id.create "stringid").Success
  let bookName = (NonEmptyString.create "notemptystr").Success
  let bookOpenRequest = 
    { id = bookId
      name = bookName
      startDate = DateTime.Now }
  let openBookCmd = OpenBook bookOpenRequest

  testList "Book aggregate should" [
    test "be able to open new book" {
      let event = execute Book.zero openBookCmd
      ShouldBe.okOf 
          <@ BookOpened @> event "Expected BookOpened event"
    }

    test "be able to apply BookOpened event" {
      let event = BookOpened bookOpenRequest
      let actual = (apply Book.zero event).Success
      let expected = 
        { id = Some bookId
          name = bookName
          startDate = bookOpenRequest.startDate
          state = Open }
      Expect.equal actual expected "Expected open book"
    }

    test "be not able to reopen closed book" {
      let book = { Book.zero with state = Closed }
      let actual = execute book openBookCmd
      ShouldBe.errorOf 
        <@ InvalidArgument @> actual 
        "Closed book cannot be reopen"
    }    

    test "be not able to open twice the same book" {
      let book = { Book.zero with state = Open }
      let actual = execute book openBookCmd
      ShouldBe.errorOf 
        <@ InvalidArgument @> actual
        "Open book cannot be reopen"
    }    
  ]
