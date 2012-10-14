\docType{package}
\name{DataFramePlus}
\alias{DataFramePlus}
\alias{DataFramePlus-class}
\alias{DataFramePlus-package}
\title{DataFramePlus}
\description{
  DataFramePlus

  Creates a new object directly extended
  \code{\link{data.frame}}, but with constrains that
  require columns. This class can be used to ensure that
  data frames have a specific structure.
}
\section{Slots}{
  \describe{ \item{\code{required}}{Object of class
  \code{character} with the names of columns that must be
  in the data frame.} \item{\code{classes}}{Object of class
  \code{character} with the classes of the columns in
  \code{required}.} \item{\code{exclusive}}{Object of class
  \code{logical}. If \code{TRUE}, then the data frame
  cannot contain any columns other than those in
  \code{required}} \item{\code{keys}}{Object of class
  \code{keys} with column names which jointly identify rows
  of the data frame.} \item{\code{constrains}}{Object of
  class \code{FunctionList}. Each function in the list
  should take one argument, and return \code{logical}.} }
}

\section{Extends}{
  \describe{ \item{\code{data.frame}}{Directly.} }
}
\examples{
new("DataFramePlus", data.frame(a=1:10),
    required="a", classes="numeric")
## Produces an error because the data frame is missing a
try(new("DataFramePlus", data.frame(foo=1:10),
        required="a", classes="numeric"))
 ## a produces NA's because it is coerced to numeric
try(new("DataFramePlus", data.frame(a=letters[1:10],
                                stringsAsFactors=FALSE),
        required="a", classes="numeric"))
 ## This is okay because exclusive is FALSE
bar <- data.frame(a=1:10, b=letters[1:10])
new("DataFramePlus", bar,
    required="a", classes="numeric")
## Removes column b if exclusive is TRUE
new("DataFramePlus", bar,
    required="a", classes="numeric",
    exclusive=TRUE)
## Produces an error because 'a' is not unique
try(new("DataFramePlus", data.frame(rep(1:10, 2)),
     required="a", classes="numeric",
     keys="a"))
}
\keyword{classes}

