#' Homogenous List
#'
#' An S4 subclass of \code{list} in which all elements of the
#' list to be the same class.
#'
#' This is similar to the 'atomic lists' in R in that all elements
#' of the vector must be the same class, but the \code{HomogList}
#' supports arbitrary classes.
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
#' }
#' 
#' @aliases HomogList-class
#' @aliases HomogList
#' @aliases [,HomogList-method
#' @aliases [<-,HomogList-method
#' @aliases [[<-,HomogList,charOrNumeric,missing-method
#' @aliases $<-,HomogList-method
#' @aliases c,HomogList-method
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
                  if (!all(sapply(object, is, class2=object@classtype))) {
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

#' @export
setMethod("c", signature="HomogList",
          def=function(x, ...) {
            y <- callGeneric(as(x, "namedList"), ...)
            new("HomogList", y, classtype=x@classtype)
          })

#' @export
setMethod("[", signature="HomogList",
          def=function(x, i, j, ...) {
              new("HomogList",
                  x@.Data[i],
                  classtype=x@classtype)
          })

#' @export
setMethod("[<-", signature="HomogList",
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), i=i, value=value)
            new("HomogList", y, classtype=x@classtype)
          })

setClassUnion("charOrNumeric", c("character", "numeric"))

setMethod("[[<-", signature=c(x="HomogList", i="charOrNumeric", j="missing", value="ANY"),
          function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), i=i, value=value)
            new("HomogList", y, classtype=x@classtype)
          })

setMethod("$<-", signature=c(x="HomogList"),
          function(x, name, value) {
            x[[name]] <- value
            new("HomogList", x)
          })

setMethod("show", "HomogList",
          function(object) {
            cat(sprintf("List of %s objects:\n", dQuote(object@classtype)))
            print(object@.Data)
          })

#' Create a subclass of HomogList
#'
#' Creates a new subclass of \code{HomogList} for a specific class.
#'
#' @param Class \code{character} string name of the new class
#' that will extend \code{HomogList}.
#' @param classtype \code{character} The name of the class which 
#' all elements must inherit from. This is tested with \code{is}.
#' @param where Passed to \code{\link{setClass}}.
#'
#' @export
#' @examples
#' FunctionList <-
#'   subclass_homog_list("FunctionList", "function")
#' # creates a new class "FunctionList")
#' showClass("FunctionList")
#' # Create a new object of class FunctionList
#' foo <- FunctionList(list(sum=sum, max=max, min=min))
#' print(foo)
#' x <- 1:10
#' lapply(foo, function(f) f(x))
#' foo[["mean"]] <- mean
#' print(foo)
#' # error
#' try(foo[["a"]] <- 1)
subclass_homog_list <- function(Class, classtype="ANY",
                                where=topenv(parent.frame())) {
    .f <- setClass(Class,
                   contains="HomogList",
                   prototype=prototype(list(), classtype=classtype),
                   where=where)

    setMethod("initialize", Class,
              function(.Object, x=list()) {
                callNextMethod(.Object, x, classtype=classtype)
              }, where=where)

    setValidity(Class,
                function(object) {
                    if (object@classtype != classtype) {
                        return(sprintf("object@classtype != %s", classtype))
                    }
                    validObject(as(object, "HomogList"))
                    TRUE
                },
                where=where)

    setMethod("[<-", Class,
              function(x, i, j, ..., value) {
                y <- callGeneric(as(x, "HomogList"), i, j, ..., value=value)
                new(Class, y)
              }, where=where)

    setMethod("[[<-", c(x=Class, i="charOrNumeric", j="missing", value="ANY"),
              function(x, i, j, ..., value) {
                y <- callGeneric(as(x, "HomogList"), i=i, value=value)
                new(Class, y)
              }, where=where)
    
    setMethod("$<-", Class,
              function(x, name, value) {
                x[[name]] <- value
                new(Class, x)
              }, where=where)
    
    setMethod("c", Class,
              function(x, ...) {
                y <- callGeneric(as(x, "HomogList"), ...)
                new(Class, y)
              }, where=where)
    
    setAs("list", Class,
          function(from, to) new(class, from), where=where)
    
    invisible(.f)
}
         

