#' @include subclass_homoglist.R
NULL

FunctionList <- subclass_homog_list("FunctionList", "function")

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
  constraints <- FunctionList(constraints)
  if (length(columns)) {
    for (i in names(columns)) {
      # hack
      if (i == "ANY") {
        next
      }
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
#' \item{\code{.Data}:}{Object of class \code{"list"}}
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
#'     \item{cbind2}{\code{signature(x = "DataFrameConstr")}: ... }
#'     \item{rbind2}{\code{signature(x = "DataFrameConstr")}: ... }
#'     \item{show}{\code{signature(object = "DataFrameConstr")}: ... }
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
#' @aliases show,DataFrameConstr-method
#' @aliases rbind2,DataFrameConstr,ANY-method
#' @aliases cbind2,DataFrameConstr,ANY-method
#' @aliases initialize,DataFrameConstr-method
#' @exportClass DataFrameConstr
#' @export
#' @examples
#' foo <- 
#'   DataFrameConstr(data.frame(a = runif(3), b = runif(3), c = letters[1:3]),
#'                   columns = c(a = "numeric", b = "ANY", c = "factor"),
#'                   constraints = list(function(x) {x$a > 0}))
#' # works just like a normal data.frame
#' print(foo)
#' summary(foo)
#' # errors
#' try(foo$a <- as.character(foo$a))
#' try(foo["a", 1] <- -1)
#' try(foo$a <- NULL)
#' # errors
#' try(foo$b <- as.character(foo$b))
#' try(foo$d <- runif(3))
DataFrameConstr <-
  setClass("DataFrameConstr", contains="data.frame",
           representation(columns="character",
                          exclusive="logical",
                          constraints="FunctionList"),
           prototype(data.frame(),
                     columns=character(),
                     exclusive=FALSE,
                     constraints=FunctionList()))

setValidity("DataFrameConstr",
            function(object) {
              rc <- validate_data_frame(object, object@columns,
                                        exclusive=object@exclusive,
                                        object@constraints)
              if (is.character(rc)) {
                return(rc)
              }
              TRUE
            })

setMethod("initialize", "DataFrameConstr",
          function(.Object,
                   x=new_data_frame(columns), columns=character(),
                   exclusive=FALSE, constraints=list()) {
              ## Drop any bad columns if exclusive
            if (exclusive) {
              coltouse <- intersect(names(x), names(columns))
              x <- as.data.frame(x)[ , coltouse, drop=FALSE]
            }
            .Object <- callNextMethod(.Object, x)
            .Object@columns <- columns
            .Object@exclusive <- exclusive
            .Object@constraints <- FunctionList(constraints)
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

#' @export
setMethod("show", "DataFrameConstr",
          function(object) {
            cat("Data frame with constraints\n")
            print(as(object, "data.frame"))
            cat("Required columns:\n")
            mapply(function(x, y) cat(sprintf("..  %s: %s\n", x, y)),
                   names(object@columns), object@columns)
            cat("Constraints:\n")
            show(object@constraints)
          })

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

