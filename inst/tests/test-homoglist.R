context("testing list-of-classes")

test_that("new(\"HomogList\", ...) with args works", {
    foo <- new("HomogList", 1:4, classtype = "integer")
    expect_is(foo, "HomogList")
    expect_equal(foo@.Data, as.list(1:4))
    expect_equal(foo@classtype, "integer")
})

test_that("new(\"HomogList\", ...) without args works", {
    foo <- new("HomogList")
    expect_is(foo, "HomogList")
    expect_equal(foo@.Data, list())
    expect_equal(foo@classtype, "ANY")
})

test_that("HomogList() works", {
  expect_identical(HomogList(1:4, "integer"),
                   new("HomogList", 1:4, "integer"))
})

test_that("Error if bad classtype", {
    expect_error(new("HomogList", letters, classtype="integer"),
                 "Not all elements have class")
})

test_that("Error if length classtype > 1", {
    expect_error(new("HomogList", 1:4, classtype=c("integer", "character")),
                 "length > 1")
})

test_that("c,HomogList-method works", {
    foo <- new("HomogList", list(a=1), classtype="numeric")
    bar <- new("HomogList", list(b=2), classtype="numeric")
    baz <- c(foo, bar)
    expect_is(baz, "HomogList")
    expect_equal(baz@.Data, as.list(1:2))
    expect_equal(names(baz), c("a", "b"))
})

test_that("[,HomogList works", {
    foo <- new("HomogList", 1:4, classtype="integer")
    bar <- foo[1:2]
    expect_is(bar, "HomogList")
    expect_equal(length(bar), 2)
})

test_that("[<-,HomogList works", {
    foo <- new("HomogList", 1:4, classtype="integer")
    foo[1:2] <- 2L:3L
    expect_is(foo, "HomogList")
    expect_equal(unname(unlist(foo)), as.integer(c(2, 3, 3, 4)))
})

test_that("[[<-,HomogList i=character works", {
  foo <- new("HomogList", list(a=1L, b=2L), classtype="numeric")
  y <- 5L
  foo[["a"]] <- y
  expect_is(foo, "HomogList")
  expect_equal(foo[["a"]], y)
})

test_that("[[<-,HomogList i=numeric works", {
  foo <- new("HomogList", 1:4, classtype="integer")
  y <- 5L
  foo[[1]] <- y
  expect_is(foo, "HomogList")
  expect_equal(foo[[1]], y)
})

test_that("$<-,HomogList works", {
  foo <- new("HomogList", list(a=1L, b=2L), classtype="integer")
  y <- 10L
  foo$a <- y
  expect_is(foo, "HomogList")
  expect_equal(foo$a, y)
})

############################################

context("HomogList subclasses")

test_that("subclass_homog_list creates a subclass", {
    foo <- subclass_homog_list("foo", "integer")
    expect_is(foo, "function")
    expect_true(isClass("foo"))
})

test_that("subclass_homog_list returned function works", {
    foo <- subclass_homog_list("foo", "integer")
    bar <- foo(1:10)
    expect_equal(bar@.Data, as.list(1:10))
    expect_equal(bar@classtype, "integer")
})

test_that("subclass_homog_list new class gives error if classtype altered", {
    foo <- subclass_homog_list("foo", "integer")
    bar <- foo(1:10)
    slot(bar, "classtype") <- "character"
    bar <- setDataPart(bar, letters)
    expect_error(validObject(bar), "object@classtype !=")
})

test_that("[<-,HomogList works", {
    foo <- subclass_homog_list("foo", "integer")
    bar <- foo(1:4)
    bar[1:2] <- 2L:3L
    expect_is(bar, "foo")
    expect_equal(unname(unlist(bar)), as.integer(c(2, 3, 3, 4)))
})

test_that("[[<-,HomogList works", {
  foo <- subclass_homog_list("foo", "integer")
  bar <- foo(1:4)
  bar[[1]] <- 2L
  expect_is(bar, "foo")
  expect_equal(unname(unlist(bar)), as.integer(c(2, 2, 3, 4)))
})

test_that("$<-,HomogList works", {
  foo <- subclass_homog_list("foo", "integer")
  bar <- foo(list(a=1L, b=2L))
  bar$a <- 10L
  expect_is(bar, "foo")
  expect_equal(bar$a, 10L)
})

test_that("c subclass works", {
  foo <- subclass_homog_list("foo", "integer")
  bar <- foo(list(a=1L, b=2L))
  baz <- c(bar, list(c=3L))
  expect_is(baz, "foo")
  expect_equal(baz@.Data, as.list(1:3))
  expect_equal(names(baz), letters[1:3])
})


