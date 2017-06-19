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

  let cannotCloseBookFromState fromState = 
      let book = { Book.zero with state = fromState }
      let event = execute book CloseBook
      ShouldBe.errorOf 
           <@ InvalidArgument @> event "Expected InvalidArgument error" 

  let cannotOpenBookFromState fromState = 
      let book = { Book.zero with state = fromState }
      let event = execute book openBookCmd
      ShouldBe.errorOf 
           <@ InvalidArgument @> event "Expected InvalidArgument error" 

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
      cannotOpenBookFromState Closed      
    }    

    test "be not able to open twice the same book" {
      cannotOpenBookFromState Open
    }

    test "be able to close opened book" {
      let book = { Book.zero with state = Open }
      let event = execute book CloseBook
      ShouldBe.okOf 
          <@ BookClosed @> event "Expected BookClosed event" 
    }

    test "be able to apply BookClosed event" {
      let actual = (apply Book.zero BookClosed).Success
      let expected = { Book.zero with state = Closed }
      Expect.equal actual expected "Expected closed book"
    }

    test "be not able to close already closed book" {
      cannotCloseBookFromState Closed
    }
    
    test "be not able to close not open book" {
      cannotCloseBookFromState NotOpenYet
    }
  ]
