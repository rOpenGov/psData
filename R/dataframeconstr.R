#' @include listofclasses.R
NULL

#' Validate \code{data.frame}: column names, classes, and arbitrary constraints
#'
#' Check that at \code{data.frame} has columns of specified classtypes, and
#' satisfies arbitrary constraints.
#'
#' @param object \code{data.frame} to be validated.
#' @param columns Named \code{character} vector. Names are required
#' @param exclusive \code{logical} If \code{TRUE}, then \code{object}
#' cannot contain any columns other than those in \code{columns}
#' columns in \code{x}, values are the classes of those columns.
#' @param constraints \code{list} of functions. Each function should
#' take only one argument, and return \code{logical}.
#' 
#' @return If valid, then \code{TRUE}, else \code{character} with
#' an error message.
#'
#' @examples
#' data(iris)
#' # check that the iris dataset has numeric column Sepal.Length
#' # and factor Species.
#' validate_data_frame(iris,
#'                     columns=c(Sepal.Length="numeric",
#'                               Species="factor"))
#' # Error because iris does not have column 'foo'
#' try(validate_data_frame(iris,
#'                     columns=c(foo="numeric")))
#' # Error because Sepal.Length is not an integer
#' try(validate_data_frame(iris,
#'                     columns=c(foo="ineger")))
#'
#' @export
validate_data_frame <- function(object, columns=NULL, exclusive=FALSE, constraints=list()) {
  if (length(columns)) {
    for (i in names(columns)) {
      if (! i %in% colnames(object)) {
        return(sprintf("column %s not in 'object'", i))
      }
      if (!is(object[[i]], columns[[i]])) {
        return(sprintf("column %s does not inherit from %s",
                       i, columns[[i]]))
      }
    }
  }
  if (exclusive) {
    othercols <- setdiff(colnames(object), names(columns))
    if (length(othercols)) {
      return(sprintf("invalid columns: %s",
                     paste(sQuote(othercols), collapse=", ")))
    }
  }
  for (f in constraints) {
    rc <- f(object)
    if (!all(rc)) {
      return(sprintf("Constraint failed:\n%s", paste(deparse(f), collapse="\n")))
    }
  }
  TRUE
}

#' Data Frame with constraints
#'
#' Creates a new object directly extended \code{\link{data.frame}},
#' but with constrains that require columns. This class can be used
#' to ensure that data frames have a specific structure.
#'
#' @param ... Data to include in the object.
#' 
#' @section Slots:
#' 
#' \describe{
#' \item{\code{list}:}{Object of class \code{"list"}}
#' \item{\code{columns}}{Named \code{character} vector. The names are the
#' column names, and the values are the required classes of the column.}
#' \item{\code{exclusive}}{Object of class \code{logical}. If \code{TRUE},
#' then the data frame cannot contain any columns other than those
#' in \code{columns}}
#' \item{\code{constraints}}{Object of class \code{list} containing \code{function}
#' elements.  Each function in the list should take one argument, and return \code{logical}.}
#' \item{\code{names}:}{Object of class \code{"character"} Column names}
#' \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} Row names}
#' \item{\code{.S3Class}:}{Object of class \code{"character"} Name of \code{S3Class}}
#' }
#'
#' @section Methods:
#'
#' Replace methods are defined to return \code{"DataFrameConstr"} objects where appropriate.
#'
#' \describe{
#'     \item{[<-}{\code{signature(x = "DataFrameConstr")}: ... }
#'     \item{[[<-}{\code{signature(x = "DataFrameConstr")}: ... }
#'     \item{$<-}{\code{signature(x = "DataFrameConstr")}: ... }
#'     \item{rbind2}{\code{signature(x = "DataFrameConstr")}: ... }
#'     \item{cbind2}{\code{signature(x = "DataFrameConstr")}: ... }
#' }
#'
#' @section Extends:
#'
#' \describe{
#' \item{\code{data.frame}}{Directly.}
#' }
#' 
#' @docType class
#' @keywords classes
#' @aliases DataFrameConstr-class
#' @aliases DataFrameConstr
#' @aliases [<-,DataFrameConstr-method
#' @aliases [[<-,DataFrameConstr,ANY,missing-method
#' @aliases [[<-,DataFrameConstr,ANY,ANY-method
#' @aliases $<-,DataFrameConstr-method
#' @aliases rbind2,DataFrameConstr,ANY-method
#' @aliases cbind2,DataFrameConstr,ANY-method
#' @aliases initialize,DataFrameConstr-method
#' @examples
#' new("DataFrameConstr", data.frame(a=1:10),
#'     columns=c(a="numeric"))
#' @export
DataFrameConstr <-
  setClass("DataFrameConstr", contains="data.frame",
           representation(columns="character",
                          exclusive="logical",
                          constraints="list"),
           prototype(data.frame(),
                     columns=character(),
                     exclusive=FALSE,
                     constraints=list()))

setValidity("DataFrameConstr",
            function(object) {
              rc <- validate_data_frame(object, object@columns, exclusive=object@exclusive,
                                        object@constraints)
              if (is.character(rc)) {
                return(rc)
              }
              TRUE
            })

setMethod("initialize", "DataFrameConstr",
          function(.Object, x, columns=character(), exclusive=FALSE, constraints=list()) {
              ## Drop any bad columns if exclusive
            if (exclusive) {
              coltouse <- intersect(names(x), names(columns))
              x <- as.data.frame(x)[ , coltouse, drop=FALSE]
            }
            .Object <- callNextMethod(.Object, x)
            .Object@columns <- columns
            .Object@exclusive <- exclusive
            .Object@constraints <- constraints
            validObject(.Object)
            .Object
          })

#' @export
setMethod("[<-", c(x="DataFrameConstr"),
          function(x, i, j, ..., value) {
            # callNextMethod() causes problems
            y <- callGeneric(data.frame(x), i, j, ..., value=value)
            new("DataFrameConstr", y,  x@columns, x@exclusive, x@constraints)
          })

#' @export
setMethod("[[<-", c(x="DataFrameConstr", i="ANY", j="missing", value="ANY"),
          function(x, i, j, ..., value) {
            y <- data.frame(x)
            y[[i]] <- value
            new("DataFrameConstr", y, x@columns, x@exclusive, x@constraints)
          })

setMethod("[[<-", c(x="DataFrameConstr", i="ANY", j="ANY", value="ANY"),
          function(x, i, j, ..., value) {
            y <- data.frame(x)
            y[[i, j]] <- value
            new("DataFrameConstr", y, x@columns, x@exclusive, x@constraints)
          })

#' @export
setMethod("$<-", "DataFrameConstr",
          function(x, name, value) {
            y <- callNextMethod()
            new("DataFrameConstr", y, x@columns, x@exclusive, x@constraints)
          })

#' @export
setMethod("rbind2", "DataFrameConstr",
          function(x, y, ...) {
            z <- rbind(as(x, "data.frame"), as(y, "data.frame"), ...)
            new("DataFrameConstr", z, x@columns, x@exclusive, x@constraints)
          })

#' @export
setMethod("cbind2", "DataFrameConstr",
          function(x, y, ...) {
            z <- cbind(as(x, "data.frame"), as(y, "data.frame"), ...)
            new("DataFrameConstr", z, x@columns, x@exclusive, x@constraints)
          })


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
#' of those columns.
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
#' foo <- subclass_data_frame_plus("foo", columns=c(a="numeric"))
#' foo(data.frame(a=1:10))
#' try(foo(data.frame(b=1:10)))
#' 
#' @export
constrained_data_frame <- function(Class, columns=character(),
                                     exclusive=FALSE,
                                     constraints=list(),
                                     where=topenv(parent.frame())) {

  .data <- data.frame()
  for (i in seq_along(columns)) {
    .data[[names(columns)[i]]] <- new(columns[i])
  }
  
  setClass(Class, contains="DataFrameConstr",
           prototype=prototype(x=.data, columns=columns,
             exclusive=exclusive,
             constraints=list()),
           where=where)
  
  setValidity(Class,
              function(object) {
                validObject(as(object, "DataFrameConstr"))
                TRUE
              },
              where=where)
  
  setMethod("initialize", Class,
              function(.Object, x) {
                callNextMethod(.Object,
                               x=x,
                               columns=columns,
                               exclusive=exclusive)
              }, where=where)

  setMethod("$<-", c(x=Class),
            function(x, name, value) {
              y <- callNextMethod()
              new(Class, y)
            }, where=where)

  setMethod("[<-", c(x=Class),
            function(x, i, j, ..., value) {
              y <- callGeneric(as(x, "DataFrameConstr"), i=i, j=j, ..., value=value)
              new(Class, y)
            }, where=where)

  ## Need to be explicitly set
  setMethod("[[<-", c(x=Class, i="ANY", j="missing", value="ANY"),
            function(x, i, j, ..., value) {
              y <- callGeneric(as(x, "DataFrameConstr"), i=i, value=value)
              new(Class, y)
            }, where=where)

  setMethod("[[<-", c(x=Class, i="ANY", j="ANY", value="ANY"),
            function(x, i, j, ..., value) {
              y <- callGeneric(as(x, "DataFrameConstr"), i=i, j=j, value=value)
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
  
  .f <- function(x) {
    new(Class, x)
  }
  invisible(.f)
}
