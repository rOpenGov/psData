context("psData")

test_that("new psData", {
  foo <- data.frame(a=letters[1:10], b=1:10)
  bar <- new("psData",
             foo,
             columns=c(a="factor", b="numeric"),
             constraints =
             list(function(x) {x$a %in% letters},
                  function(x) {x$b <= 26}))
  expect_is(bar, "psData")
})

test_that("new psData without args works", {
  foo <- data.frame(a=letters[1:10], b=1:10)
  bar <- new("psData")
  expect_is(bar, "psData")
})

test_that("new psData with only columns works", {
  bar <- new("psData", columns=c(a="integer"))
  expect_is(bar, "psData")
})

test_that("psData with column class ANY works", {
  bar <- new("psData", data.frame(a=1:10), columns=c(a="ANY"))
  expect_is(bar, "psData")
})

test_that("psData inherits from data.frame", {
  foo <- new("psData",
             data.frame(a=letters[1:10]))
  expect_is(foo, "data.frame")
})

test_that("error: missing column", {
  expect_error(new("psData",
                   data.frame(a=letters[1:10]),
                   columns=c(a="factor", b="numeric")),
               regexp="column b not in 'object'")
})

test_that("error: bad column type", {
  expect_error(new("psData",
                   data.frame(a=letters[1:10], b=letters[1:10]),
                   columns=c(a="factor", b="numeric")),
               regexp="column b does not inherit from numeric")
})

test_that("extra columns and exclusive=TRUE produces no error and drops extra cols", {
 foo <-  new("psData",
             data.frame(a=letters[1:10], b=letters[1:10]),
             columns=c(a="factor"),
             exclusive=TRUE)
 expect_equal(colnames(foo), c("a"))
})

test_that("extra columns and exclusive=FALSE produces no error error and does not drop extra cols", {
 foo <-  new("psData",
             data.frame(a=letters[1:10], b=letters[1:10]),
             columns=c(a="factor"),
             exclusive=FALSE)
 expect_equal(colnames(foo), c("a", "b"))
})
          
test_that("constraints throws error", {
  expect_error(new("psData",
                   data.frame(a=LETTERS[1:10], b=1:10),
                   columns=c(a="factor", b="numeric"),
                   constraints = list(function(x) {all(x$b <= 5)})),
               regexp="Constraint failed")
})

######################

## Test Methods

foo <- psData(data.frame(a=1:5, b=6:10),
                       columns=c(a="numeric", b="numeric"))

###

context("[,psData-method")

test_that("[,psData,missing,missing works", {
  expect_equal(foo[], foo)
})

test_that("[,psData,missing,character with drop=missing  works", {
  expect_equal(foo[ , "a"], as.numeric(1:5))
})

test_that("[,psData,missing,character with drop=FALSE works", {
  expect_equal(foo[ , "a", drop=FALSE], data.frame(a=1:5))
})

test_that("[,psData,missing,character with drop=FALSE works", {
  expect_equal(foo[ , "a", drop=FALSE], data.frame(a=1:5))
})

test_that("[,psData,integer,mssing works", {
  expected <- psData(data.frame(a=1:2, b=6:7),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo[1:2], expected)
})

test_that("[,psData,integer,mssing: test #1", {
  expect_equal(foo[1:2, "a"], 1:2)
})

test_that("[,psData,integer,mssing: test #2", {
  expect_equal(foo[1:2, "a", drop=FALSE], data.frame(a=1:2))
})

test_that("[,psData,integer,mssing: test #3", {
  expected <- psData(data.frame(a=1:2, b=6:7),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo[1:2, c("a", "b"), drop=FALSE], expected)
})

########
context("[<-,psData method")

test_that("[<-,psData,missing,missing: working", {
  foo[] <- 1
  expected <- psData(data.frame(a=rep(1, 5), b=rep(1, 5)),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo, expected)
})

test_that("[<-,psData,missing,missing: error", {
  expect_error(foo[] <- "a", "invalid class")
})

test_that("[<-,psData,missing,ANY: working", {
  foo[ , "b"] <- 11:15
  expected <- psData(data.frame(a=1:5, b=11:15),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo, expected)
})

test_that("[<-,psData,missing,ANY: error", {
  expect_error(foo[ , "b"] <- "a", "invalid class")
})

test_that("[<-,psData,ANY,missing: works", {
  foo[1, ] <- 100
  expected <- psData(data.frame(a=c(100, 2:5), b=c(100, 7:10)),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo, expected)
})

test_that("[<-,psData,missing,ANY: error", {
  expect_error(foo[1] <- "a", "invalid class")
})

test_that("[<-,psData,ANY,ANY: works", {
  foo[1, "a"] <- 100
  expected <- psData(data.frame(a=c(100, 2:5), b=6:10),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo, expected)
})

test_that("[<-,psData,ANY,ANY: error", {
  expect_error(foo[1, "a"] <- "a", "invalid class")
})

####
context("[[<-,psData method")

test_that("[[<-,psData,missing,missing error", {
  expect_error(foo[[]] <- 1, "missing subscript")
})

test_that("[[<-,psData,integer,missing works", {
  foo[[1]] <- 100
  expected <- psData(data.frame(a=rep(100, 5), b=6:10),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo, expected)
})

test_that("[[<-,psData,character,missing works", {
  foo[["a"]] <- 100
  expected <- psData(data.frame(a=rep(100, 5), b=6:10),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo, expected)
})

test_that("[[<-,psData,ANY,missing error", {
  expect_error(foo[["a"]] <- "a", "invalid class")
})

test_that("[[<-,psData,integer,integer,missing works", {
  foo[[1, 1]] <- 100
  expected <- psData(data.frame(a=c(100, 2:5), b=6:10),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo, expected)
})

test_that("[[<-,psData,integer,character,missing works", {
  foo[[1, "a"]] <- 100
  expected <- psData(data.frame(a=c(100, 2:5), b=6:10),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo, expected)
})

test_that("[[<-,psData,ANY,ANY error", {
  expect_error(foo[[1, "a"]] <- "a", "invalid class")
})

####
context("$<-,psData method")

test_that("$<-,psData works", {
  foo$a <- 11:15
  expected <- psData(data.frame(a=11:15, b=6:10),
                              columns=c(a="numeric", b="numeric"))
  expect_equal(foo, expected)
})

test_that("$<-,psData error", {
  expect_error(foo$a <- "a", "invalid class")
})

####
context("colnames<-,psData method")

test_that("colnames<- error", {
  expect_error(colnames(foo) <- c("d", "b"))
})

test_that("colnames<- works", {
  colnames(foo) <- c("a", "b")
  expect_is(foo, "psData")
})

####
context("rownames<-,psData method")

test_that("rownames<- works", {
  rownames(foo) <- letters[1:5]
  expect_is(foo, "psData")
})

test_that("rownames<- with NULL works", {
  rownames(foo) <- NULL
  expect_is(foo, "psData")
})

####
context("names<-,psData method")

test_that("names<- works", {
  names(foo) <- c("a", "b")
  expect_is(foo, "psData")
})

test_that("names<- error", {
  expect_error(names(foo) <- c("c", "b"), "invalid class")
})

####
context("dimnames<-,psData method")

test_that("dimnames<- works", {
  dimnames(foo) <- list(1:5, c("a", "b"))
  expect_is(foo, "psData")
})

test_that("dimnames<- error", {
  expect_error(dimnames(foo) <- list(1:5, c("d", "b")), "invalid class")
})

