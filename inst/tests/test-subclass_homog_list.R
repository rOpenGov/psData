context("subclass_homog_list")

test_that("subclass_homog_list creates a subclass", {
  foo <- subclass_homog_list("foo", "integer")
  expect_is(foo, "function")
  expect_true(isClass("foo"))
})

test_that("subclass_homog_list works as expected", {
  Foo <- subclass_homog_list("Foo", "numeric")
  foo <- Foo(list(a=1, b=2))
  expect_is(foo, "Foo")
  expect_equal(foo@.Data, list(1, 2))
  expect_equal(names(foo), c("a", "b"))
  expect_equal(foo@classtype, "numeric")
})

test_that("subclass_homog_list new class gives error if classtype altered", {
  Foo <- subclass_homog_list("Foo", "numeric")
  foo <- Foo(list(a=1, b=2))
  slot(foo, "classtype") <- "character"
  expect_error(validObject(foo), "invalid class")
})

###############

Foo <- subclass_homog_list("Foo", "numeric")
foo <- Foo(list(a=1, b=2))

#######

context("HomogList c-method")

test_that("c-method: test #1", {
  bar <- Foo(list(c=3))
  baz <- c(foo, bar)
  expect_equal(baz, Foo(list(a=1, b=2, c=3)))
})

test_that("c-method: test #2", {
  bar <- list(c=3)
  baz <- c(foo, bar)
  expect_equal(baz, Foo(list(a=1, b=2, c=3)))
})

test_that("c-method: test #3", {
  bar <- list(d="4")
  expect_error(c(foo, bar), "invalid class")
})


#######

context("HomogList [-method")

test_that("[-method integer works", {
  expect_equal(foo[2], Foo(list(b=2)))
})

test_that("[-method character works", {
  expect_equal(foo["b"], Foo(list(b=2)))
})

test_that("[-method missing works", {
  expect_equal(foo[], foo)
})

#######

context("[<- method")

test_that("[<- with missing,vector works", {
  foo[] <- 3
  expected <- Foo(list(a=3, b=3))
  expect_equal(foo, expected)
})

test_that("[<- with missing,list works", {
  foo[] <- list(a=3, b=4)
  expected <- Foo(list(a=3, b=4))
  expect_equal(foo, expected)
})

test_that("[<- with character: test #1", {
  foo["a"] <- 100
  expected <- Foo(list(a=100, b=2))
  expect_equal(foo, expected)
})

test_that("[<- with integer: test #1", {
  foo[1] <- c(100)
  expected <- Foo(list(a=100, b=2))
  expect_equal(foo, expected)
})

##########
context("[[<- method")

test_that("[[<- with missing throws error", {
  expect_error({foo[[]] <- 1}, regexp="missing subscript")
})

test_that("[[<- with character", {
  foo[["a"]] <- 100
  expect_equal(foo, Foo(list(a=100, b=2)))
})

test_that("[[<- with numeric", {
  foo[[1]] <- 100
  expect_equal(foo, Foo(list(a=100, b=2)))
})

#########

context("$<- method")

test_that("$<-,HomogList works", {
  foo$a <- 100
  expect_equal(foo, Foo(list(a=100, b=2)))
})

########

context("names<- method")

test_that("names<- with character works", {
  names(foo) <- c("d", "e")
  expect_equal(foo, Foo(list(d=1, e=2)))
})

test_that("names<- with NULL works", {
  names(foo) <- NULL
  expect_equal(foo, Foo(list(1, 2)))
})

###########

context("length<- method")

test_that("length<- works with value < length(object)", {
  length(foo) <- 1
  expect_equal(foo, Foo(list(a=1)))
})

test_that("length<- works with value == length(object)", {
  length(foo) <- length(foo)
  expect_equal(foo, Foo(list(a=1, b=2)))
})

test_that("length<- works with value > length(object)", {
  length(foo) <- length(foo) + 1
  expect_is(foo, "Foo")
  expect_equal(foo@.Data, list(1, 2, NULL))
  expect_equal(foo@names, c("a", "b", ""))
})
