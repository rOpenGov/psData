#' @include package.R
#' @include class-DataFrameConstr.R
#' @export contrained_data_frame
NULL

new_data_frame <- function(columns=character()) {
  .data <- data.frame()
  for (i in seq_along(columns)) {
    cname <- names(columns)[i]
    classname <- columns[i]
    if (classname == "ANY") {
      .data[[cname]] <- numeric()
    } else {
      .data[[cname]] <- new(classname)
   }
  }
  .data
}

#' Create subclasss of \code{DataFrameConstr}
#'
#' This function creates a class which directly extends
#' \code{DataFrameConstr} with the requirement that the slots
#' (\code{columns}, and \code{exclusive}
#' take specific values.
#'
#' @param Class \code{character} Name of the new class.
#' @param columns Named \code{character} vector. The names are
#' the names of required columns; the values are the classes
#' of those columns.  Use \code{ANY} to allow a class
#' to be anything.
#' @param exclusive \code{logical} If \code{TRUE}, then
#' the data frame can only contain the columns in \code{columns}.
#' @param constraints \code{list} of functions. Each function should
#' take only one argument, and return \code{logical}.
#' @param where \code{environment}. The environment in which to store
#' the definition. See \code{\link{setClass}}.
#' @return Invisibly returns a constructor function for the
#' new class.
#'
#' @examples
#' Foo <-
#'   constrained_data_frame("Foo",
#'                          columns = c(a = "numeric", b = "ANY", c = "factor"),
#'                          constraints = list(function(x) {x$a > 0}))
#' showClass("Foo")
#' 
#' # Create a new "Foo" object
#' foo <- Foo(data.frame(a = runif(3), b = runif(3), c = letters[1:3]))
#' # this also works
#' # new("Foo", data.frame(a = runif(3), b = runif(3), c = letters[1:3]))
#' # works like a normal data.frame
#' print(foo)
#' summary(foo)
#' # errors
#' try(foo$a <- as.character(foo$a))
#' try(foo["a", 1] <- -1)
#' try(foo$a <- NULL)
#' # errors
#' try(foo$b <- as.character(foo$b))
#' try(foo$d <- runif(3))
constrained_data_frame <- function(Class, columns=character(),
                                     exclusive=FALSE,
                                     constraints=list(),
                                     where=topenv(parent.frame())) {

  constraints <- FunctionList(constraints)
  
  setClass(Class, contains="DataFrameConstr",
           prototype=
           prototype(x=new_data_frame(columns), columns=columns,
                     exclusive=exclusive,
                     constraints=constraints),
           where=where)
  
  setValidity(Class,
              function(object) {
                validObject(as(object, "DataFrameConstr"))
              },
              where=where)

  setMethod("initialize", Class,
            function(.Object, x=new_data_frame(columns)) {
              callNextMethod(.Object, x=x,
                             columns=columns,
                             exclusive=exclusive,
                             constraints=constraints
                             )
            }, where=where)

  setMethod("$<-", c(x=Class),
            function(x, name, value) {
              y <- callNextMethod()
              new(Class, y)
            }, where=where)

  setMethod("[<-", c(x=Class),
            function(x, i, j, value) {
              y <- callGeneric(as(x, "DataFrameConstr"), i, j, value=value)
              new(Class, y)
            }, where=where)

  setMethod("[", c(x=Class),
            function(x, i, j, drop) {
              y <- callGeneric(as(x, "DataFrameConstr"), i, j, drop=drop)
              if (is(y, "DataFrameConstr")) {
                new(Class, y)
              } else {
                y
              }
            }, where=where)
  
  ## Need to be explicitly set
  setMethod("[[<-", c(x=Class, i="ANY", j="missing", value="ANY"),
            function(x, i, j, value) {
              y <- callGeneric(as(x, "DataFrameConstr"), i, value=value)
              new(Class, y)
            }, where=where)

  setMethod("[[<-", c(x=Class, i="ANY", j="ANY", value="ANY"),
            function(x, i, j, value) {
              y <- callGeneric(as(x, "DataFrameConstr"), i, j, value=value)
              new(Class, y)
            }, where=where)
  
  setMethod("rbind2", Class,
            function(x, y, ...) {
              z <- callNextMethod()
              new(Class, z)
            }, where=where)

  setMethod("cbind2", Class,
            function(x, y, ...) {
              z <- callNextMethod()
              new(Class, z)
            }, where=where)
  
  setAs("data.frame", Class,
        function(from, to) new(Class, from), where=where)
  
  .f <- function(...) {
    new(Class, ...)
  }
  invisible(.f)
}
