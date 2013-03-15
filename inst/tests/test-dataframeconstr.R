context("DataFrameConstr")

test_that("new DataFrameConstr", {
  foo <- data.frame(a=letters[1:10], b=1:10)
  bar <- new("DataFrameConstr",
             foo,
             columns=c(a="factor", b="numeric"),
             constraints =
             list(function(x) {x$a %in% letters},
                  function(x) {x$b <= 26}))
  expect_is(bar, "DataFrameConstr")
})

test_that("new DataFrameConstr without args works", {
  foo <- data.frame(a=letters[1:10], b=1:10)
  bar <- new("DataFrameConstr")
  expect_is(bar, "DataFrameConstr")
})

test_that("new DataFrameConstr with only columns works", {
  bar <- new("DataFrameConstr", columns=c(a="integer"))
  expect_is(bar, "DataFrameConstr")
})

test_that("DataFrameConstr with column class ANY works", {
  bar <- new("DataFrameConstr", data.frame(a=1:10), columns=c(a="ANY"))
  expect_is(bar, "DataFrameConstr")
})

test_that("DataFrameConstr inherits from data.frame", {
  foo <- new("DataFrameConstr",
             data.frame(a=letters[1:10]))
  expect_is(foo, "data.frame")
})

test_that("error: missing column", {
  expect_error(new("DataFrameConstr",
                   data.frame(a=letters[1:10]),
                   columns=c(a="factor", b="numeric")),
               regexp="column b not in 'object'")
})

test_that("error: bad column type", {
  expect_error(new("DataFrameConstr",
                   data.frame(a=letters[1:10], b=letters[1:10]),
                   columns=c(a="factor", b="numeric")),
               regexp="column b does not inherit from numeric")
})

test_that("extra columns and exclusive=TRUE produces no error and drops extra cols", {
 foo <-  new("DataFrameConstr",
             data.frame(a=letters[1:10], b=letters[1:10]),
             columns=c(a="factor"),
             exclusive=TRUE)
 expect_equal(colnames(foo), c("a"))
})

test_that("extra columns and exclusive=FALSE produces no error error and does not drop extra cols", {
 foo <-  new("DataFrameConstr",
             data.frame(a=letters[1:10], b=letters[1:10]),
             columns=c(a="factor"),
             exclusive=FALSE)
 expect_equal(colnames(foo), c("a", "b"))
})
          
test_that("constraints throws error", {
  expect_error(new("DataFrameConstr",
                   data.frame(a=LETTERS[1:10], b=1:10),
                   columns=c(a="factor", b="numeric"),
                   constraints = list(function(x) {all(x$b <= 5)})),
               regexp="Constraint failed")
})

test_that("[<- works", {
  foo <- DataFrameConstr(data.frame(a=1:10),
                       columns=c(a="integer"))
  foo[1, "a"] <- 5L
  expect_is(foo, "DataFrameConstr")
  expect_equal(foo[1, "a"], 5L)
})

test_that("[<- throws error if invalid object produced", {
  foo <- DataFrameConstr(data.frame(a=1:10),
                       columns=c(a="integer"))
  expect_error(foo["a", 1] <- "c",
               "column a does not inherit from integer")
})

test_that("[ works", {
  foo <- DataFrameConstr(data.frame(a=1:10, b=1:10),
                       columns=c(a="integer", b="numeric"))
  expect_is(foo[1:2, ], "DataFrameConstr")
})

test_that("[ does not return DataFrameConst or error if invalid", {
  foo <- DataFrameConstr(data.frame(a=1:10, b=1:10),
                         columns=c(a="integer", b="numeric"))[ , "a"]
  expect_false(is(foo, "DataFrameConstr"))
})

test_that("[[<- with i=ANY, j=missing works", {
  foo <- DataFrameConstr(data.frame(a=1:10),
                       columns=c(a="integer"))
  y <- foo$a + 20L
  foo[["a"]] <- y
  expect_is(foo, "DataFrameConstr")
  expect_equal(foo[["a"]], y)
})

test_that("[[<- with i=ANY, j=ANY works", {
  foo <- DataFrameConstr(data.frame(a=1:10),
                       columns=c(a="integer"))
  y <- 10L
  foo[[1, "a"]] <- y
  expect_is(foo, "DataFrameConstr")
  expect_equal(foo[[1, "a"]], y)
})

test_that("[[<- throws error if invalid object produced", {
  foo <- DataFrameConstr(data.frame(a=1:10),
                       columns=c(a="integer"))
  expect_error(foo[["a"]] <- letters[seq_len(nrow(foo))],
               "column a does not inherit from integer")
})

test_that("$<- works", {
  foo <- DataFrameConstr(data.frame(a=1:10),
                       columns=c(a="integer"))
  foo$a <- 1:10 + 10L
  expect_is(foo, "DataFrameConstr")
  expect_equal(foo$a, 1:10 + 10)
})

test_that("$<- throws error if invalid object produced", {
  foo <- DataFrameConstr(data.frame(a=1:10),
                       columns=c(a="integer"))
  expect_error(foo$a <- letters[seq_len(nrow(foo))],
               "column a does not inherit from integer")
})

test_that("dimnames<- works", {
  foo <- DataFrameConstr(data.frame(a=1:10, b=1:10),
                       columns=c(a="integer"))
  dimnames(foo)[[2]] <- c("a", "c")
  expect_is(foo, "DataFrameConstr")
  expect_equal(dimnames(foo), list(as.character(1:10), c("a", "c")))
})

test_that("dim<- works", {
  foo <- DataFrameConstr(data.frame(a=1:10, b=1:10),
                       columns=c(a="integer"))
  names(foo) <- c("a", "c")
  expect_is(foo, "DataFrameConstr")
  expect_equal(dimnames(foo), list(as.character(1:10), c("a", "c")))
})

test_that("rbind2 works", {
  foo <- DataFrameConstr(data.frame(a=1:10, b=1:10),
                       columns=c(a="integer"))
  bar <- rbind2(foo, data.frame(a=13L, b=14L))
  expect_is(bar, "DataFrameConstr")
})

test_that("cbind2 works", {
  foo <- DataFrameConstr(data.frame(a=1:10, b=1:10),
                       columns=c(a="integer"))
  bar <- cbind2(foo, data.frame(c=1))
  expect_is(bar, "DataFrameConstr")
})

