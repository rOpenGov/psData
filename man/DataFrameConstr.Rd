\docType{class}
\name{DataFrameConstr}
\alias{$<-,DataFrameConstr-method}
\alias{DataFrameConstr}
\alias{DataFrameConstr-class}
\alias{[<-,DataFrameConstr-method}
\alias{[[<-,DataFrameConstr,ANY,ANY-method}
\alias{[[<-,DataFrameConstr,ANY,missing-method}
\alias{cbind2,DataFrameConstr,ANY-method}
\alias{initialize,DataFrameConstr-method}
\alias{rbind2,DataFrameConstr,ANY-method}
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
  \describe{ \item{\code{list}:}{Object of class
  \code{"list"}} \item{\code{columns}}{Named
  \code{character} vector. The names are the column names,
  and the values are the required classes of the column.}
  \item{\code{exclusive}}{Object of class \code{logical}.
  If \code{TRUE}, then the data frame cannot contain any
  columns other than those in \code{columns}}
  \item{\code{constraints}}{Object of class
  \code{FunctionList} containing \code{function} elements.
  Each function in the list should take one argument, and
  return \code{logical}.} \item{\code{names}:}{Object of
  class \code{"character"} Column names}
  \item{\code{row.names}:}{Object of class
  \code{"data.frameRowLabels"} Row names}
  \item{\code{.S3Class}:}{Object of class
  \code{"character"} Name of \code{S3Class}} }
}

\section{Methods}{
  Replace methods are defined to return
  \code{"DataFrameConstr"} objects where appropriate.

  \describe{ \item{[<-}{\code{signature(x =
  "DataFrameConstr")}: ... } \item{[[<-}{\code{signature(x
  = "DataFrameConstr")}: ... } \item{$<-}{\code{signature(x
  = "DataFrameConstr")}: ... }
  \item{rbind2}{\code{signature(x = "DataFrameConstr")}:
  ... } \item{cbind2}{\code{signature(x =
  "DataFrameConstr")}: ... } }
}

\section{Extends}{
  \describe{ \item{\code{data.frame}}{Directly.} }
}
\examples{
new("DataFrameConstr", data.frame(a=1:10),
    columns=c(a="numeric"))
}
\keyword{classes}

