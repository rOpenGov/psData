context("testing dataframeplus")

test_that("new DataFramePlus", {
  foo <- data.frame(a=letters[1:10], b=1:10)
  bar <- new("DataFramePlus",
             foo,
             columns=c(a="factor", b="numeric"),
             constraints =
             list(function(x) {x$a %in% letters},
                  function(x) {x$b <= 26}))
  expect_is(bar, "DataFramePlus")
})

test_that("DataFramePlus inherits from data.frame", {
  foo <- new("DataFramePlus",
             data.frame(a=letters[1:10]))
  expect_is(foo, "data.frame")
})

test_that("error: missing column", {
  expect_error(new("DataFramePlus",
                   data.frame(a=letters[1:10]),
                   columns=c(a="factor", b="numeric")),
               regexp="column b not in 'object'")
})

test_that("error: bad column type", {
  expect_error(new("DataFramePlus",
                   data.frame(a=letters[1:10], b=letters[1:10]),
                   columns=c(a="factor", b="numeric")),
               regexp="column b does not have class numeric")
})

test_that("extra columns and exclusive=TRUE produces no error and drops extra cols", {
 foo <-  new("DataFramePlus",
             data.frame(a=letters[1:10], b=letters[1:10]),
             columns=c(a="factor"),
             exclusive=TRUE)
 expect_equal(colnames(foo), c("a"))
})

test_that("extra columns and exclusive=FALSE produces no error error and does not drop extra cols", {
 foo <-  new("DataFramePlus",
             data.frame(a=letters[1:10], b=letters[1:10]),
             columns=c(a="factor"),
             exclusive=FALSE)
 expect_equal(colnames(foo), c("a", "b"))
})
          
test_that("constraints throws error", {
  expect_error(new("DataFramePlus",
                   data.frame(a=LETTERS[1:10], b=1:10),
                   columns=c(a="factor", b="numeric"),
                   constraints = list(function(x) {all(x$b <= 5)})),
               regexp="Constraint failed")
})

test_that("[<- works", {
  foo <- DataFramePlus(data.frame(a=1:10),
                       columns=c(a="integer"))
  foo["a", 1] <- 5L
  expect_is(foo, "DataFramePlus")
})

test_that("[<- throws error if invalid object produced", {
  foo <- DataFramePlus(data.frame(a=1:10),
                       columns=c(a="integer"))
  expect_error(foo["a", 1] <- "c",
               "column a does not inherit from integer")
})

test_that("[[<- works", {
  foo <- DataFramePlus(data.frame(a=1:10),
                       columns=c(a="integer"))
  foo[["a"]] <- 1:5
  expect_is(foo, "DataFramePlus")
})

test_that("[[<- throws error if invalid object produced", {
  foo <- DataFramePlus(data.frame(a=1:10),
                       columns=c(a="integer"))
  expect_error(foo[["a"]] <- letters[seq_len(nrow(foo))],
               "column a does not inherit from integer")
})

test_that("Subclassing works", {
  subclass_data_frame_plus("df1", columns=c(a="factor"))
  expect_is(new("df1", data.frame(a=letters)), "df1")
})

test_that("Subclassing returns a function", {
  df1 <- subclass_data_frame_plus("df1", columns=c(a="factor"))
  expect_is(df1(data.frame(a=letters)), "df1")
})

