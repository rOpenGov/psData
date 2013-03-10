context("testing list-of-classes")

test_that("test list of classes", {
    foo <- new("ListOfClasses", 1:4, classtype = "integer")
    expect_is(foo, "ListOfClasses")
    expect_equal(foo@.Data, as.list(1:4))
    expect_equal(foo@classtype, "integer")
})

test_that("Error if bad classtype", {
    expect_error(new("ListOfClasses", letters, classtype="integer"),
                 "Not all elements have class")
})

test_that("Error if length classtype > 1", {
    expect_error(new("ListOfClasses", 1:4, classtype=c("integer", "character")),
                 "length > 1")
})

test_that("c,ListOfClasses-method works", {
    foo <- new("ListOfClasses", 1:4, classtype="integer")
    bar <- new("ListOfClasses", 5:10, classtype="integer")
    baz <- c(foo, bar)
    expect_is(baz, "ListOfClasses")
    expect_equal(baz@.Data, as.list(1:10))
    expect_equal(baz@classtype, "integer")
})

test_that("[,ListOfClasses works", {
    foo <- new("ListOfClasses", 1:4, classtype="integer")
    bar <- foo[1:2]
    expect_is(bar, "ListOfClasses")
    expect_equal(length(bar), 2)
})

test_that("[<-,ListOfClasses works", {
    foo <- new("ListOfClasses", 1:4, classtype="integer")
    foo[1:2] <- 2L:3L
    expect_is(foo, "ListOfClasses")
    expect_equal(unlist(foo), as.integer(c(2, 3, 3, 4)))
})

test_that("[[<-,ListOfClasses works", {
  foo <- new("ListOfClasses", 1:4, classtype="integer")
  foo[[1]] <- 2L
  expect_is(foo, "ListOfClasses")
  expect_equal(foo[[1]], 2L)
})

test_that("subclass_list_of_classes creates a subclass", {
    foo <- subclass_list_of_classes("foo", "integer")
    expect_is(foo, "function")
    expect_true(isClass("foo"))
})

test_that("subclass_list_of_classes returned function works", {
    foo <- subclass_list_of_classes("foo", "integer")
    bar <- foo(1:10)
    expect_equal(bar@.Data, as.list(1:10))
    expect_equal(bar@classtype, "integer")
})

test_that("subclass_list_of_classes new class gives error if classtype altered", {
    foo <- subclass_list_of_classes("foo", "integer")
    bar <- foo(1:10)
    slot(bar, "classtype") <- "character"
    bar <- setDataPart(bar, letters)
    expect_error(validObject(bar), "object@classtype !=")
})

