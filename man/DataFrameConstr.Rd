\docType{class}
\name{DataFrameConstr}
\alias{$<-,DataFrameConstr-method}
\alias{DataFrameConstr}
\alias{DataFrameConstr-class}
\alias{[,DataFrameConstr,ANY,ANY-method}
\alias{[,DataFrameConstr,ANY,missing-method}
\alias{[,DataFrameConstr,missing,ANY-method}
\alias{[,DataFrameConstr,missing,missing-method}
\alias{[<-,DataFrameConstr,ANY,ANY-method}
\alias{[<-,DataFrameConstr,ANY,missing-method}
\alias{[<-,DataFrameConstr,missing,ANY-method}
\alias{[<-,DataFrameConstr,missing,missing-method}
\alias{[[<-,DataFrameConstr,ANY,ANY-method}
\alias{[[<-,DataFrameConstr,ANY,missing-method}
\alias{cbind2,DataFrameConstr,ANY-method}
\alias{colnames<-,DataFrameConstr-method}
\alias{dimnames<-,DataFrameConstr,list-method}
\alias{initialize,DataFrameConstr-method}
\alias{names<-,DataFrameConstr,ANY-method}
\alias{rbind2,DataFrameConstr,ANY-method}
\alias{rownames<-,DataFrameConstr,ANY-method}
\alias{rownames<-,DataFrameConstr,NULL-method}
\alias{show,DataFrameConstr-method}
\title{Data Frame with constraints}
\usage{
  DataFrameConstr(...)
}
\arguments{
  \item{...}{Data to include in the object.}
}
\description{
  Creates a new object directly extended
  \code{\link{data.frame}}, but with constrains that
  require columns. This class can be used to ensure that
  data frames have a specific structure.
}
\section{Slots}{
  \describe{ \item{\code{.Data}:}{Object of class
  \code{"list"}} \item{\code{columns}}{Named
  \code{character} vector. The names are the column names,
  and the values are the required classes of the column.}
  \item{\code{exclusive}}{Object of class \code{logical}.
  If \code{TRUE}, then the data frame cannot contain any
  columns other than those in \code{columns}}
  \item{\code{constraints}}{Object of class \code{list}
  containing \code{function} elements.  Each function in
  the list should take one argument, and return
  \code{logical}.} \item{\code{names}:}{Object of class
  \code{"character"} Column names}
  \item{\code{row.names}:}{Object of class
  \code{"data.frameRowLabels"} Row names}
  \item{\code{.S3Class}:}{Object of class
  \code{"character"} Name of \code{S3Class}} }
}

\section{Methods}{
  Methods commonly used with data frames are defined to
  return \code{"DataFrameConstr"} objects where
  appropriate, or throw errors if they would create an
  invalid \code{"DataFrameConstr"} object.

  \describe{ \item{[<-}{\code{signature(x =
  "DataFrameConstr")}: } \item{[[<-}{\code{signature(x =
  "DataFrameConstr")}: } \item{[}{\code{signature(object =
  "DataFrameConstr")}: Returns
  \linkS4class{DataFrameConstr} if the returned object is
  valid, otherwise returns a \code{data.frame}.  }
  \item{$<-}{\code{signature(x = "DataFrameConstr")}: }
  \item{cbind2}{\code{signature(x = "DataFrameConstr")}:}
  \item{rbind2}{\code{signature(x = "DataFrameConstr")}:
  ... } \item{names<-}{\code{signature(x =
  "DataFrameConstr")}: ... }
  \item{colnames<-}{\code{signature(object =
  "DataFrameConstr")}: }
  \item{rownames<-}{\code{signature(object =
  "DataFrameConstr")}: }
  \item{dimnames<-}{\code{signature(object =
  "DataFrameConstr")}: } }
}

\section{Extends}{
  \describe{ \item{\code{data.frame}}{Directly.} }
}
\examples{
foo <-
  DataFrameConstr(data.frame(a = runif(3), b = runif(3), c = letters[1:3]),
                  columns = c(a = "numeric", b = "ANY", c = "factor"),
                  constraints = list(function(x) {x$a > 0}))
# works just like a normal data.frame
print(foo)
summary(foo)
# errors
try(foo$a <- as.character(foo$a))
try(foo["a", 1] <- -1)
try(foo$a <- NULL)
# errors
try(foo$b <- as.character(foo$b))
try(foo$d <- runif(3))
}
\keyword{classes}

