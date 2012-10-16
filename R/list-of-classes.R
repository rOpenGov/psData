##' List of classes
##'
##' An S4 subclass of \code{list} which requires all elements of the
##' list to be the same class.
##' 
##' @section Slots:
##'
##' \describe{
##' \item{\code{.Data}}{Object as \code{list}.}
##' \item{\code{classtype}}{Object as \code{character}. Required classtype for
##' all elements in the list.}
##' }
##'
##' @section Extends:
##'
##' \describe{
##' \item{\code{list}}{Directly.}
##' }
##'
##' @export
setClass("ListOfClasses",
         contains="list",
         representation(classtype="character"))

##' @export
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

##' @export
setMethod("c", signature="ListOfClasses",
          def=function(x, ...) {
              new("ListOfClasses",
                  c(x@.Data, ...),
                  classtype=x@classtype)
          })


##' @export
subclass_list_of_classes <- function(class, classtype="ANY",
                                     where=topenv(parent.frame())) {
    setClass(class,
             contains="ListOfClasses",
             prototype=prototype(list(), classtype=classtype),
             where=where)

    setValidity(class,
                function(object) {
                    if (object@classtype != classtype) {
                        return(sprintf("object@classtype != %s", classtype))
                    }
                    validObject(as(object, "ListOfClasses"))
                    TRUE
                },
                where=where)

    setMethod("c", class, 
          function(x, ...) {
              new(class, callNextMethod(x, ...))
          })
    
    setAs("list", class,
          function(from, to) new(class, from), where=where)

    .f <- function(x) {
        new(class, x)
    }
    invisible(.f)
}
         
