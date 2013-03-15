context("constrained_data_frame")

test_that("Subclassing works", {
  constrained_data_frame("df1", columns=c(a="factor"))
  expect_is(new("df1", data.frame(a=letters)), "df1")
})

test_that("Subclassing returns a function", {
  df1 <- constrained_data_frame("df1", columns=c(a="factor"))
  expect_is(df1(data.frame(a=letters)), "df1")
})

test_that("Subclass [ returns subclass if valid", {
  df1 <- constrained_data_frame("df1",
                                columns=c(a="numeric", b="numeric"))
  foo <- df1(data.frame(a=1:3, b=1:3))
  expect_is(foo[1:2, ], "df1")
})

test_that("Subclass [ does not throw error if invalid", {
  df1 <- constrained_data_frame("df1",
                                columns=c(a="numeric", b="numeric"))
  foo <- df1(data.frame(a=1:3, b=1:3))
  expect_false(is(foo[ , "a"], "df1"))
})

test_that("Subclass [[<- with i=ANY, j=missing works", {
  df1 <- constrained_data_frame("df1", columns=c(a="numeric"))
  foo <- df1(data.frame(a=1:10))
  y <- foo$a + 10
  foo[["a"]] <- y
  expect_is(foo, "df1")
  expect_equal(foo[["a"]], y)
})

test_that("Subclass [[<- with i=ANY, j=ANY works", {
  df1 <- constrained_data_frame("df1", columns=c(a="numeric"))
  foo <- df1(data.frame(a=1:10))
  y <- 10L
  foo[[1, "a"]] <- y
  expect_is(foo, "df1")
  expect_equal(foo[[1, "a"]], y)
})

test_that("Subclassing [<- with i=ANY, j=ANY works", {
  df1 <- constrained_data_frame("df1", columns=c(a="numeric"))
  foo <- df1(data.frame(a=1:10))
  y <- 10L
  foo[1, "a"] <- y
  expect_is(foo, "df1")
  expect_equal(foo[1, "a"], y)
})

test_that("Subclass [<- with i=ANY, j=missing works", {
  df1 <- constrained_data_frame("df1", columns=c(a="numeric"))
  foo <- df1(data.frame(a=1:10))
  y <- foo$a + 10L
  foo[, "a"] <- y
  expect_is(foo, "df1")
  expect_equal(foo[, "a"], y)
})

test_that("Subclass $<- works", {
  df1 <- constrained_data_frame("df1", columns=c(a="numeric"))
  foo <- df1(data.frame(a=1:10))
  y <- foo$a + 10L
  foo$a <- y
  expect_is(foo, "df1")
  expect_equal(foo[, "a"], y)
})

test_that("Subclass cbind2 works", {
  df1 <- constrained_data_frame("df1", columns=c(a="numeric"))
  foo <- df1(data.frame(a=1:10))
  bar <- cbind2(foo, data.frame(c=11))
  expect_is(bar, "df1")
  expect_equal(dim(bar), c(nrow(foo), 2))
})

test_that("Subclass rbind2 works", {
  df1 <- constrained_data_frame("df1", columns=c(a="numeric"))
  foo <- df1(data.frame(a=1:10))
  bar <- rbind2(foo, data.frame(a=11))
  expect_is(bar, "df1")
  expect_equal(dim(bar), c(nrow(foo) + 1, 1))
})

