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
                 "length != 1")
})

#############################################
## Methods

foo <- new("HomogList", list(a=1, b=2, c=3), classtype="numeric")

#######

context("HomogList c-method")

test_that("c-method: Homoglist,Homoglist works", {
  bar <- new("HomogList", list(d=4), classtype="numeric")
  baz <- c(foo, bar)
  expect_equal(baz, new("HomogList", list(a=1, b=2, c=3, d=4), classtype="numeric"))
})

test_that("c-method: Homoglist,list works", {
  bar <- new("HomogList", list(d=4), classtype="numeric")
  baz <- c(foo, bar)
  expect_equal(baz, new("HomogList", list(a=1, b=2, c=3, d=4), classtype="numeric"))
})

test_that("c-method: Homoglist,list throws error if bad", {
  bar <- list(d="4")
  expect_error(c(foo, bar), "invalid class")
})


#######

context("HomogList [-method")

test_that("[-method HomogList,integer works", {
  expect_equal(foo[2], new("HomogList", list(b=2), classtype="numeric"))
})

test_that("[-method HomogList,character works", {
  expect_equal(foo["b"], new("HomogList", list(b=2), classtype="numeric"))
})

test_that("[-method HomogList,missing works", {
  expect_equal(foo[], foo)
})

#######

context("[<- method")

test_that("[<- with HomogList,missing,vector works", {
  foo[] <- 3
  expected <- new("HomogList", list(a=3, b=3, c=3), classtype="numeric")
  expect_equal(foo, expected)
})

test_that("[<- with HomogList,missing,list works", {
  foo[] <- list(a=3, b=2, c=1)
  expected <- new("HomogList", list(a=3, b=2, c=1), classtype="numeric")
  expect_equal(foo, expected)
})

test_that("[<- with HomogList,character: test #1", {
  foo["a"] <- 100
  expected <- new("HomogList", list(a=100, b=2, c=3), classtype="numeric")
  expect_equal(foo, expected)
})

test_that("[<- with HomogList,character: test #2", {
  foo[c("a", "b")] <- c(100, 200)
  expected <- new("HomogList", list(a=100, b=200, c=3), classtype="numeric")
  expect_equal(foo, expected)
})

test_that("[<- with HomogList,integer: test #1", {
  foo[1] <- c(100)
  expected <- new("HomogList", list(a=100, b=2, c=3), classtype="numeric")
  expect_equal(foo, expected)
})

test_that("[<- with HomogList,integer: test #2", {
  foo[2:3] <- c(100, 200)
  expected <- new("HomogList", list(a=1, b=100, c=200), classtype="numeric")
  expect_equal(foo, expected)
})

##########

test_that("[[<- with HomogList, missing throws error", {
  expect_error({foo[[]] <- 1}, regexp="missing subscript")
})

test_that("[[<-,HomogList,character: test #1", {
  foo[["a"]] <- 100
  expect_equal(foo, new("HomogList", list(a=100, b=2, c=3), classtype="numeric"))
})

test_that("[[<-,HomogList,character: test #2", {
  foo[[c("a", "b")]] <- 100
  expect_equal(foo, new("HomogList", list(a=c(100, b=100), b=2, c=3), classtype="numeric"))
})

test_that("[[<-,HomogList,numeric: test #1", {
  foo[[1]] <- 100
  expect_equal(foo, new("HomogList", list(a=100, b=2, c=3), classtype="numeric"))
})

test_that("[[<-,HomogList,numeric: test #2", {
  foo[[c(1, 2)]] <- 100
  expect_equal(foo, new("HomogList", list(a=c(1, 100), b=2, c=3), classtype="numeric"))
})

#########

context("$<- method")

test_that("$<-,HomogList works", {
  foo$a <- 100
  expect_equal(foo, new("HomogList", list(a=100, b=2, c=3), classtype="numeric"))
})

########

context("names<- method")

test_that("names<- with character works", {
  names(foo) <- c("d", "e", "f")
  expect_equal(foo, new("HomogList", list(d=1, e=2, f=3), classtype="numeric")) 
})

test_that("names<- with NULL works", {
  names(foo) <- NULL
  expect_equal(foo, new("HomogList", list(1, 2, 3), classtype="numeric"))
})

###########

context("length<- method")

test_that("length<- works with value < length(object)", {
  length(foo) <- 1
  expect_equal(foo, new("HomogList", list(a=1), "numeric"))
})

test_that("length<- works with value == length(object)", {
  length(foo) <- length(foo)
  expect_equal(foo, new("HomogList", list(a=1, b=2, c=3), "numeric"))
})

test_that("length<- works with value > length(object)", {
  length(foo) <- length(foo) + 1
  expect_is(foo, "HomogList")
  expect_equal(foo@.Data, list(1, 2, 3, NULL))
  expect_equal(foo@names, c("a", "b", "c", ""))
})
