context("subclass_homog_list")

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


