#' @include package.R
#' @exportClass HomogList
#' @export HomogList
#' @exportMethod [
#' @exportMethod [<-
#' @exportMethod [[<-
#' @exportMethod $<-
#' @exportMethod c
#' @exportMethod names<-
#' @exportMethod length<-
NULL

is_or_null <- function(object, class2) {
  is(object, class2) || is.null(object)
}

#' Homogenous List
#'
#' An S4 subclass of \code{list} in which all elements of the
#' list to be the same class.
#'
#' This is similar to the 'atomic lists' in R in that all elements
#' of the vector must be the same class, but the \code{HomogList}
#' supports arbitrary classes. \code{NULL} values are also valid.
#' 
#' @section Slots:
#'
#' \describe{
#' \item{\code{.Data}}{Object of class \code{list}.}
#' \item{\code{classtype}}{Object of class \code{character}. Required classtype for
#' all elements in the list.}
#' }
#'
#' @param ... Passed onto generic functions.
#'
#' @section Extends:
#'
#' \describe{
#' \item{\code{list}}{Directly.}
#' }
#'
#' @section Methods:
#' 
#' \describe{
#'     \item{[<-}{\code{signature(x = "HomogList")}: ... }
#'     \item{[}{\code{signature(x = "HomogList")}: ... }
#'     \item{[[<-}{\code{signature(x = "HomogList")}: ... }
#'     \item{c}{\code{signature(x = "HomogList")}: ... }
#'     \item{show}{\code{signature(object = "HomogList")}: ... }
#' }
#' 
#' @aliases HomogList-class
#' @aliases HomogList
#' @aliases [,HomogList,missing-method
#' @aliases [,HomogList,ANY-method
#' @aliases [<-,HomogList,missing-method
#' @aliases [<-,HomogList,ANY-method
#' @aliases [[<-,HomogList,missing,ANY-method
#' @aliases [[<-,HomogList,ANY,ANY-method
#' @aliases $<-,HomogList-method
#' @aliases c,HomogList-method
#' @aliases length<-,HomogList,numeric-method
#' @aliases names<-,HomogList,NULL-method
#' @aliases show,HomogList-method
#' @docType class
#' @keywords classes
#' @exportClass HomogList
#' @export
#' @examples
#' foo <- HomogList(list(sum=sum, max=max, min=min), "function")
#' print(foo)
#' x <- 1:10
#' lapply(foo, function(f) f(x))
#' foo[["mean"]] <- mean
#' print(foo)
#' # error
#' try(foo[["a"]] <- 1)
HomogList <- setClass("HomogList",
                      contains="namedList",
                      representation(classtype="character"),
                      prototype(list(), classtype="ANY"))

setValidity("HomogList",
            function(object) {
                if (length(object@classtype) != 1) {
                    return("object@classtype has a length != 1")
                }
                # Hack. need to test s3 and s4 classes differently
                # is(x, "ANY") does not work for s3 objects
                if (object@classtype != "ANY") {
                  if (!all(sapply(object, is_or_null, class2=object@classtype))) {
                    return(sprintf("Not all elements have class %s",
                                   object@classtype))
                  }
                }
                TRUE
              })

setMethod("initialize", "HomogList",
          function(.Object, x=list(), classtype="ANY") {
            .Object <- callNextMethod(.Object, x)
            .Object@classtype <- classtype
            validObject(.Object)
            .Object
          })

setMethod("show", "HomogList",
          function(object) {
            cat(sprintf("List of %s objects\n", dQuote(object@classtype)))
            print(structure(object@.Data, names = object@names))
          })

### Methods


setMethod("c", signature="HomogList",
          def=function(x, ...) {
            y <- callGeneric(as(x, "namedList"), ...)
            new("HomogList", y, classtype=x@classtype)
          })

setMethod("[", signature=c(x="HomogList", i="missing"), 
          function(x, i, j, ...., drop) x)

setMethod("[", signature=c(x="HomogList", i="ANY"), 
          function(x, i, j, ...., drop) {
            y <- callGeneric(as(x, "namedList"), i=i)
            new("HomogList", y, classtype=x@classtype)
          })

setMethod("[<-", signature=c(x="HomogList", i="missing"), 
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), value=value)
            new("HomogList", y, classtype=x@classtype)
          })

setMethod("[<-", signature=c(x="HomogList", i="ANY"), 
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), i, value=value)
            new("HomogList", y, classtype=x@classtype)
          })

setMethod("[[<-", signature=c(x="HomogList", i="missing", value="ANY"),
          function(x, i, j, ..., value) {
            # Error ... : [[ ]] with missing subscript
            callGeneric(as(x, "namedList"), value=value)
          })

setMethod("[[<-", signature=c(x="HomogList", i="ANY", value="ANY"),
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), i=i, value=value)
            new("HomogList", y, classtype=x@classtype)
          })

setMethod("$<-", signature=c(x="HomogList"),
          function(x, name, value) {
            x[[name]] <- value
            x
          })

setMethod("names<-", signature=c(x="HomogList", value="NULL"),
          function(x, value) {
            x@names <- rep(NA_character_, length(x))
            x
          })

setMethod("length<-", signature=c(x="HomogList", value="numeric"),
          function(x, value) {
            y <- callGeneric(as(x, "namedList"), value)
            new("HomogList", y, classtype=x@classtype)
          })

