setClass("ListOfClasses", "list",
         representation(classtype="character"))

setValidity("ListOfClasses",
            function(object) {
                if (length(classtype) > 1) {
                    return("object@classtype has a length > 1")
                }
                if (!all(object, is, class2=object@classtype)) {
                    return(sprintf("Not all elements have class %s",
                                   object@classtype))
                }
                TRUE
            })

setMethod("c", "ListOfClasses",
          function(x, ...) {
              new("ListOfClasses", callNextMethod(x, ...),
                  classtype=x@classtype)
          })

subclass_list_of_classes <- function(class, classtype="ANY",
                                     where=topenv(parent.frame())) {
    setClass(class, contains="ListOfClasses",
             prototype=prototype(list(), classtype=classtype),
             where=where)

    setValidity(class,
                function(object) {
                    if (object@classtype != classtype) {
                        return(sprintf("object@classtype != %s", classtype))
                    }
                    callNextMethod(object)
                    TRUE
                },
                where=where)

    setMethod("c", class, 
          function(x, ...) {
              new(class, callNextMethod(x, ...))
          })
    
    setAs("list", class,
          function(from, to) new(class, from), where=where)

    setMethod("c", class, 
    .f <- function(x) {
        new(class, x)
    }
    invisible(.f)
}
         
