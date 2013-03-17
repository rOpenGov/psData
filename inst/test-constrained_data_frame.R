context("constrained_data_frame")

test_that("Subclassing works", {
  constrained_data_frame("df1", columns=c(a="factor"))
  expect_is(new("df1", data.frame(a=letters)), "df1")
})

test_that("Subclassing returns a function", {
  df1 <- constrained_data_frame("df1", columns=c(a="factor"))
  expect_is(df1(data.frame(a=letters)), "df1")
})

################

Foo <- constrained_data_frame("Foo", columns=c(a="numeric", b="numeric"))
foo <- Foo(data.frame(a=1:5, b=6:10))

####

context("subclass DataFrameConst [ method")

test_that("[,DataFrameConstr,missing,missing works", {
  expect_equal(foo[], foo)
})

test_that("[,DataFrameConstr,missing,character with drop=missing  works", {
  expect_equal(foo[ , "a"], as.numeric(1:5))
})

test_that("[,DataFrameConstr,missing,character with drop=FALSE works", {
  expect_equal(foo[ , "a", drop=FALSE], data.frame(a=1:5))
})

test_that("[,DataFrameConstr,missing,character with drop=FALSE works", {
  expect_equal(foo[ , "a", drop=FALSE], data.frame(a=1:5))
})

test_that("[,DataFrameConstr,integer,mssing works", {
  expected <- Foo(data.frame(a=1:2, b=6:7))
  expect_equal(foo[1:2], expected)
})

test_that("[,DataFrameConstr,integer,mssing: test #1", {
  expect_equal(foo[1:2, "a"], 1:2)
})

test_that("[,DataFrameConstr,integer,mssing: test #2", {
  expect_equal(foo[1:2, "a", drop=FALSE], data.frame(a=1:2))
})

test_that("[,DataFrameConstr,integer,mssing: test #3", {
  expected <- Foo(data.frame(a=1:2, b=6:7))
  expect_equal(foo[1:2, c("a", "b"), drop=FALSE], expected)
})

### 
