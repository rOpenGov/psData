#' @include package.R
#' @include homoglist.R
#' @export subclass_data_frame
NULL

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
