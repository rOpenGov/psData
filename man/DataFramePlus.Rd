\docType{class}
\name{DataFramePlus}
\alias{$<-,DataFramePlus-method}
\alias{DataFramePlus}
\alias{DataFramePlus-class}
\alias{DataFramePlus-package}
\alias{[<-,DataFramePlus-method}
\alias{[[<-,DataFramePlus,ANY,ANY-method}
\alias{[[<-,DataFramePlus,ANY,missing-method}
\alias{cbind2,DataFramePlus,ANY-method}
\alias{initialize,DataFramePlus-method}
\alias{rbind2,DataFramePlus,ANY-method}
\title{Data Frame with constraints}
\usage{
  DataFramePlus(...)
}
\arguments{
  \item{...}{Data to include in the object.}
}
\description{
  Creates a new object directly extended
  \code{\link{data.frame}}, but with constrains that
  require columns. This class can be used to ensure that
  data frames have a specific structure.

  DataFramePlus
}
\section{Slots}{
  \describe{ \item{\code{list}:}{Object of class
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
  Replace methods are defined to return
  \code{"DataFramePlus"} objects where appropriate.

  \describe{ \item{[<-}{\code{signature(x =
  "DataFramePlus")}: ... } \item{[[<-}{\code{signature(x =
  "DataFramePlus")}: ... } \item{$<-}{\code{signature(x =
  "DataFramePlus")}: ... } \item{rbind2}{\code{signature(x
  = "DataFramePlus")}: ... }
  \item{cbind2}{\code{signature(x = "DataFramePlus")}: ...
  } }
}

\section{Extends}{
  \describe{ \item{\code{data.frame}}{Directly.} }
}
\examples{
new("DataFramePlus", data.frame(a=1:10),
    columns=c(a="numeric"))
}
\keyword{classes}

