\name{subclass_homog_list}
\alias{subclass_homog_list}
\title{Create a subclass of HomogList}
\usage{
  subclass_homog_list(Class, classtype = "ANY",
    where = topenv(parent.frame()))
}
\arguments{
  \item{Class}{\code{character} string name of the new
  class that will extend \code{HomogList}.}

  \item{classtype}{\code{character} The name of the class
  which all elements must inherit from. This is tested with
  \code{is}.}

  \item{where}{Passed to \code{\link{setClass}}.}
}
\description{
  Creates a new subclass of \code{HomogList} for a specific
  class.
}
\examples{
FunctionList <-
  subclass_homog_list("FunctionList", "function")
# creates a new class "FunctionList")
showClass("FunctionList")
# Create a new object of class FunctionList
foo <- FunctionList(list(sum=sum, max=max, min=min))
print(foo)
x <- 1:10
lapply(foo, function(f) f(x))
foo[["mean"]] <- mean
print(foo)
# error
try(foo[["a"]] <- 1)
}

