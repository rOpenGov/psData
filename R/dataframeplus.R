#' @include listofclasses.R
NULL

FunctionList <- subclass_list_of_classes("FunctionList", "function")

#' Check column names and classes of a \code{data.frame}
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

#' Data Frame with column constraints
#'
#' Creates a new object directly extended \code{\link{data.frame}},
#' but with constrains that require columns. This class can be used
#' to ensure that data frames have a specific structure.
#'
#' @param ... Passed to \link{new}
#'

#' @section Slots:
#' 
#' \describe{
#' \item{\code{names}:}{Object of class \code{"character"} ~~ }
#' \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} ~~ }
#' \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
#' \item{\code{columns}}{Named \code{character} vector. The names are the
#' column names, and the values are the required class of the column.}
#' \item{\code{exclusive}}{Object of class \code{logical}. If \code{TRUE},
#' then the data frame cannot contain any columns other than those
#' in \code{columns}}
#' \item{\code{constraints}}{Object of class \code{FunctionList}. Each function in
#' the list should take one argument, and return \code{logical}.}
#' }
#'
#' @section Methods:
#' \describe{
#'     \item{[<-}{\code{signature(x = "DataFramePlus")}: ... }
#'     \item{[[<-}{\code{signature(x = "DataFramePlus")}: ... }
#'     \item{initialize}{\code{signature(.Object = "DataFramePlus")}: ... }
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
#' @aliases DataFramePlus-class
#' @aliases DataFramePlus
#' @aliases [<-,DataFramePlus-method
#' @aliases [[<-,DataFramePlus-method
#' @aliases initialize,DataFramePlus-method
#' @examples
#' new("DataFramePlus", data.frame(a=1:10),
#'     columns=c(a="numeric"))
#' @export
DataFramePlus <-
  setClass("DataFramePlus", contains="data.frame",
           representation(columns="character",
                          exclusive="logical",
                          constraints="FunctionList"),
           prototype(data.frame(),
                     columns=character(),
                     exclusive=FALSE,
                     constraints=FunctionList(list())))

setValidity("DataFramePlus",
            function(object) {
              rc <- validate_data_frame(object, object@columns, exclusive=object@exclusive,
                                        object@constraints)
              if (is.character(rc)) {
                return(rc)
              }
              TRUE
            })

setMethod("initialize", "DataFramePlus",
          function(.Object, x, columns, exclusive=FALSE, constraints=list()) {
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
setMethod("[<-", c(x="DataFramePlus"),
          function(x, i, j, ..., value) {
            # was having trouble with call next function working
            new("DataFramePlus", `[<-`(data.frame(x), i, j, ..., value=value),
                x@columns, x@exclusive, x@constraints)
          })

#' @export
setMethod("[[<-", "DataFramePlus",
          function(x, i, ..., value) {
            new("DataFramePlus", `[[<-`(data.frame(x), i, value=value),
                x@columns, x@exclusive, x@constraints)
          })


#' Create subclasss of \code{DataFramePlus}
#'
#' This function creates a class which directly extends
#' \code{DataFramePlus} with the requirement that the slots
#' (\code{columns}, and \code{exclusive}
#' take specific values.
#'
#' @param class \code{character} Name of the new class.
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
subclass_data_frame_plus <- function(class, columns=character(),
                                     exclusive=FALSE,
                                     where=topenv(parent.frame()),
                                     constraints=list()) {

  .data <- data.frame()
  for (i in seq_along(columns)) {
    .data[[names(columns)[i]]] <- new(columns[i])
  }
  
  setClass(class, contains="DataFramePlus",
           prototype=prototype(x=.data, columns=columns,
             exclusive=exclusive,
             constraints=FunctionList(constraints)),
           where=where)
  
  setValidity(class,
              function(object) {
                validObject(as(object, "DataFramePlus"))
                TRUE
              },
              where=where)
  
  setMethod("initialize", class,
              function(.Object, x) {
                callNextMethod(.Object,
                               x=x,
                               columns=columns,
                               exclusive=exclusive)
              }, where=where)
  
  setAs("data.frame", class,
        function(from, to) new(class, from), where=where)
  
  .f <- function(x) {
    new(class, x)
  }
  invisible(.f)
}
