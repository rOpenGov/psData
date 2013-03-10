#' List of classes
#'
#' An S4 subclass of \code{list} which requires all elements of the
#' list to be the same class.
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
#'     \item{[<-}{\code{signature(x = "ListOfClasses")}: ... }
#'     \item{[}{\code{signature(x = "ListOfClasses")}: ... }
#'     \item{[[<-}{\code{signature(x = "ListOfClasses")}: ... }
#'     \item{c}{\code{signature(x = "ListOfClasses")}: ... }
#' }
#' 
#' @aliases ListOfClasses-class
#' @aliases ListOfClasses
#' @aliases [,ListOfClasses-method
#' @aliases [<-,ListOfClasses-method
#' @aliases [[<-,ListOfClasses-method
#' @aliases $<-,ListOfClasses-method
#' @aliases c,ListOfClasses-method
#' @docType class
#' @keywords classes
#' @exportClass ListOfClasses
#' @export
ListOfClasses <- setClass("ListOfClasses",
                          contains="namedList",
                          representation(classtype="character"))

setValidity("ListOfClasses",
            function(object) {
                if (length(object@classtype) > 1) {
                    return("object@classtype has a length > 1")
                }
                if (!all(sapply(object, is, class2=object@classtype))) {
                    return(sprintf("Not all elements have class %s",
                                   object@classtype))
                }
                TRUE
            })

#' @export
setMethod("c", signature="ListOfClasses",
          def=function(x, ...) {
              new("ListOfClasses",
                  c(x@.Data, ...),
                  classtype=x@classtype)
          })

#' @export
setMethod("[", signature="ListOfClasses",
          def=function(x, i, j, ...) {
              new("ListOfClasses",
                  x@.Data[i],
                  classtype=x@classtype)
          })

#' @export
setMethod("[<-", signature="ListOfClasses",
          def = function(x, i, j, ..., value) {
            y <- callGeneric(as(x, "namedList"), i, j, ..., value=value)
            new("ListOfClasses", y, classtype=x@classtype)
          })

#' @export
## setMethod("$<-", signature=c(x="ListOfClasses"),
##           def = function(x, name, value) {
##             what <- substitute(name)
##             if (is.symbol(what)) {
##               what <- as.character(what)
##             } else {
##               what <- name
##             }
##             x[[what]] <- value
##             x
##         })

## #' @export
## setMethod("[[<-", signature=c(x="ListOfClasses"),
##           function(x, i, j, ..., value) {
##             y <- as(x, "namedList")
##             y[[i]] <- value
##             new("ListOfClasses", y, classtype=x@classtype)
##           })


#' Create subclass list of classes
#'
#' Creates a new subclass of \code{ListOfClasses} which requires a
#' specific class.
#'
#' @param Class \code{character} string name for the class.
#' @param classtype \code{character} The name of the class which 
#' all elements must inherit from. This is tested with \code{is}.
#' @param where Passed to \code{\link{setClass}}.
#' 
#' @export
subclass_list_of_classes <- function(Class, classtype="ANY",
                                     where=topenv(parent.frame())) {
    .f <- setClass(Class,
                   contains="ListOfClasses",
                   prototype=prototype(classtype=classtype),
                   where=where)

    setValidity(Class,
                function(object) {
                    if (object@classtype != classtype) {
                        return(sprintf("object@classtype != %s", classtype))
                    }
                    validObject(as(object, "ListOfClasses"))
                    TRUE
                },
                where=where)

    setMethod("[<-", Class,
              function(x, i, j, ..., value) {
                y <- callGeneric(as(x, "ListOfClasses"), i, j, ..., value=value)
                new(Class, y)
              }, where=where)

    setMethod("[[<-", Class,
              function(x, i, j, ..., value) {
                y <- callGeneric(as(x, "ListOfClasses"), i, j, ..., value=value)
                new(Class, y)
              }, where=where)

    setMethod("$<-", Class,
              function(x, name, value) {
                ## rewrite to use partial matching as in a list
                what <- substitute(name)
                if (is.symbol(what)) 
                  what <- as.character(what)
                else what <- name
                x[[what]] <- value
                x
              }, where=where)
    
    setAs("namedList", Class,
          function(from, to) new(class, from), where=where)

    invisible(.f)
}
         

