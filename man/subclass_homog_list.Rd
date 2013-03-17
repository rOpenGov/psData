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
NumericList <-
  subclass_homog_list("NumericList", "numeric")
# creates a new class "NumericList"
showClass("NumericList")
# Create a new object of class NumericList
foo <- NumericList(list(a=1, b=2))
print(foo)
foo[["c"]] <- 3
print(foo)
# error
try(foo[["c"]] <- 3)
}

