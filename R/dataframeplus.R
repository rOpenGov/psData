##' Check whether columns could be a key to a data.frame
##' 
##' @param x \code{data.frame} to check
##' @param i \code{character} or \code{integer} Column names
##' @return \code{logical}. \code{TRUE} if no duplicated values
##' of \code{i} in \code{x}.
##' @export
is_key <- function(x, i) {
    !any(duplicated(x[ , i]))
}

##' Check column names and classes of a \code{data.frame}
##'
##' @param object \code{data.frame} to be validated.
##' @param columns Named \code{character} vector. Names are required
##' @param exclusive \code{logical} If \code{TRUE}, then \code{object}
##' cannot contain any columns other than those in \code{columns}
##' columns in \code{x}, values are the classes of those columns.
##' @param keys \code{character} Names of columns which should be
##' jointly unique.
##' @return If valid, then \code{TRUE}, else \code{character} with
##' an error message.
##' @export
validate_data_frame <- function(object, columns, exclusive=FALSE, keys=character()) {
    # error checking functions
    if (any(! keys %in% names(columns))) {
        stop("Keys must be in columns")
    }
    # actual 
    for (i in names(columns)) {
        if (! i %in% colnames(object)) {
            return(sprintf("column %s not in 'object'", i))
        }
        if (!is(object[[i]], columns[[i]])) {
            return(sprintf("column %s does not have class %s",
                           i, columns[[i]]))
        }
    }
    if (exclusive) {
        othercols <- setdiff(colnames(object), names(columns))
        if (length(othercols)) {
            return(sprintf("invalid columns: %s",
                           paste(sQuote(othercols), collapse=", ")))
        }
    }
    if (length(keys)) {
        if (! is_key(object, keys)) {
            return(sprintf("columns %s are not unique",
                           paste(sQuote(keys), collapse=", ")))
        }
    }
    TRUE
}

##' Data Frame with column constraints
##'
##' Creates a new object directly extended \code{\link{data.frame}},
##' but with constrains that require columns. This class can be used
##' to ensure that data frames have a specific structure.
##'
##' @section Slots:
##' 
##' \describe{
##' \item{\code{required}}{Object of class \code{character} with
##' the names of columns that must be in the data frame.}
##' \item{\code{classes}}{Object of class \code{character} with
##' the classes of the columns in \code{required}.}
##' \item{\code{exclusive}}{Object of class \code{logical}. If \code{TRUE},
##' then the data frame cannot contain any columns other than those
##' in \code{required}}
##' \item{\code{keys}}{Object of class \code{keys} with column names which jointly
##' identify rows of the data frame.}
##' }
##'
##' @section Extends:
##'
##' \describe{
##' \item{\code{data.frame}}{Directly.}
##' }
##' 
##' @docType class
##' @keywords classes
##' @aliases DataFramePlus-class
##' @examples
##' new("DataFramePlus", data.frame(a=1:10),
##'     required="a", classes="numeric")
##' ## Produces an error because the data frame is missing a
##' try(new("DataFramePlus", data.frame(foo=1:10),
##'         required="a", classes="numeric"))
##'  ## a produces NA's because it is coerced to numeric
##' try(new("DataFramePlus", data.frame(a=letters[1:10],
##'                                 stringsAsFactors=FALSE),
##'         required="a", classes="numeric"))
##'  ## This is okay because exclusive is FALSE
##' bar <- data.frame(a=1:10, b=letters[1:10])
##' new("DataFramePlus", bar,
##'     required="a", classes="numeric")
##' ## Removes column b if exclusive is TRUE
##' new("DataFramePlus", bar,
##'     required="a", classes="numeric",
##'     exclusive=TRUE)
##' ## Produces an error because 'a' is not unique
##' try(new("DataFramePlus", data.frame(rep(1:10, 2)),
##'      required="a", classes="numeric",
##'      keys="a"))
##' @export
setClass("DataFramePlus", contains="data.frame",
         representation(required="character",
                        classes="character",
                        exclusive="logical",
                        keys="character"),
         prototype(data.frame(),
                   required = character(),
                   classes=character(),
                   exclusive=FALSE,
                   keys=character()))

setValidity("DataFramePlus",
            function(object) {
                if(length(object@required) != length(object@classes)) {
                    return("length(object@required) != length(object@classes)")
                }
                validate_data_frame(object, setNames(object@classes, object@required),
                                    exclusive=object@exclusive, keys = object@keys)
            })

setMethod("initialize", "DataFramePlus",
          function(.Object, x, required=character(), classes=character(), exclusive=FALSE, keys=character()) {
              if(length(required) != length(classes)) {
                  return("length(object@required) != length(object@classes)")
              }
              ## Drop any bad columns if exclusive
              if (exclusive) {
                  coltouse <- intersect(names(x), required)
                  x <- as.data.frame(x)[ , coltouse, drop=FALSE]
              }
              ## Coerce column types
              ## for (i in seq_along(required)) {
              ##     nm <- required[i]
              ##     cls <- classes[i]
              ##     x[[nm]] <- as(x[[nm]], cls)
              ## }
              .Object <- callNextMethod(.Object, x)
              .Object@required <- required
              .Object@classes <- classes
              .Object@keys <- keys
              .Object@exclusive <- exclusive
              validObject(.Object)
              .Object
          })

##' Create subclasss of \code{DataFramePlus}
##'
##' This function creates a class which directly extends
##' \code{DataFramePlus} with the requirement that the slots
##' (\code{required}, \code{classes}, \code{exclusive}, and
##' \code{keys}) take specific values.
##'
##' @param class \code{character} Name of the new class.
##' @param columns Named \code{character} vector. The names are
##' the names of required columns; the values are the classes
##' of those columns.
##' @param exclusive \code{logical} If \code{TRUE}, then
##' the data frame can only contain the columns in \code{columns}.
##' @param keys \code{character} Columns in the data fram which
##' a jointly unique.
##' 
##' @return Invisibly returns a constructor function for the
##' new class.
##'
##' @examples
##' \dontrun{
##' foo <- subclass_data_frame_plus("foo", columns=c(a="numeric"))
##' foo(data.frame(a=1:10))
##' try(foo(data.frame(b=1:10)))
##' }
##' 
##' @export
subclass_data_frame_plus <- function(class, columns=character(),
                                     exclusive=FALSE,
                                     keys=character()) {

    required <- names(columns)
    if (is.null(required)) {
        required <- character()
    }
    classes <- unname(columns)
    
    .data <- list()
    for (i in seq_along(required)) {
        .data[[required[i]]] <- new(classes[i])
    }
    .data <- data.frame(.data)
    
    setClass(class, contains="DataFramePlus",
             prototype=prototype(x=.data, required=required,
                       classes=classes, exclusive=exclusive,
                       keys=keys))

    setValidity(class,
                function(object) {
                    if (!setequal(object@required, required)) {
                        return(sprintf("object@required: %s != %s",
                                       deparse(object@required), deparse(required)))
                    }
                    if (!setequal(object@classes, classes)) {
                        return(sprintf("object@classes: %s != %s",
                                       deparse(object@classes), deparse(classes)))
                    }
                    if (object@exclusive != exclusive) {
                        return(sprintf("exclusive: != %s", exclusive))
                    }
                    if (!setequal(object@keys, keys)) {
                        return(sprintf("object@keys: %s != %s",
                                       deparse(object@keys), deparse(keys)))
                    }
                    validObject(as(object, "DataFramePlus"))
                    TRUE
                })
    
    setMethod("initialize", class,
              function(.Object, x) {
                  if (missing(x)) {
                      x <- .data
                  }
                  .Object <- callNextMethod(.Object, x=x, required=required,
                                            classes=classes,
                                            exclusive=exclusive, keys=keys)
                  .Object
              })

    setAs("data.frame", class,
          function(from, to) new(class, from))
    
    .f <- function(x) {
        new(class, x)
    }
    invisible(.f)
}
