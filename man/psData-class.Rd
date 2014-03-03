\docType{class}
\name{psData-class}
\alias{psData-class}
\title{Data Frame with constraints}
\arguments{
  \item{...}{Data to include in the object.}
}
\description{
Creates a new object directly extended
\code{\link{data.frame}}, but with constrains that require
columns. This class can be used to ensure that data frames
have a specific structure.
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
  return \code{"psData"} objects where appropriate, or
  throw errors if they would create an invalid
  \code{"psData"} object.

  \describe{ \item{[<-}{\code{signature(x = "psData")}: }
  \item{[[<-}{\code{signature(x = "psData")}: }
  \item{[}{\code{signature(object = "psData")}: Returns
  \linkS4class{psData} if the returned object is valid,
  otherwise returns a \code{data.frame}.  }
  \item{$<-}{\code{signature(x = "psData")}: }
  \item{cbind2}{\code{signature(x = "psData")}:}
  \item{rbind2}{\code{signature(x = "psData")}: ... }
  \item{names<-}{\code{signature(x = "psData")}: ... }
  \item{colnames<-}{\code{signature(object = "psData")}: }
  \item{rownames<-}{\code{signature(object = "psData")}: }
  \item{dimnames<-}{\code{signature(object = "psData")}: }
  }
}

\section{Extends}{
  \describe{ \item{\code{data.frame}}{Directly.} }
}
\examples{
foo <-
  psData(data.frame(a = runif(3), b = runif(3), c = letters[1:3]),
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

