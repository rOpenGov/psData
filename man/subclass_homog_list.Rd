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
# create a list of list of lists
ListOfLists <- subclass_homog_list("ListOfLists", "list")
foo <- ListOfLists(list(list(a=1), list(b=2))
# Error because 2 is not a list
try(ListOfLists(list(list(a=1), 2))
}

