\docType{class}
\name{DataFramePlus}
\alias{$<-,DataFramePlus-method}
\alias{DataFramePlus}
\alias{DataFramePlus-class}
\alias{DataFramePlus-package}
\alias{[<-,DataFramePlus-method}
\alias{[[<-,DataFramePlus-method}
\alias{initialize,DataFramePlus-method}
\title{Data Frame with column constraints}
\usage{
  DataFramePlus(...)
}
\arguments{
  \item{...}{Passed to \link{new}}
}
\description{
  Creates a new object directly extended
  \code{\link{data.frame}}, but with constrains that
  require columns. This class can be used to ensure that
  data frames have a specific structure.

  DataFramePlus
}
\section{Slots}{
  \describe{ \item{\code{names}:}{Object of class
  \code{"character"} ~~ } \item{\code{row.names}:}{Object
  of class \code{"data.frameRowLabels"} ~~ }
  \item{\code{.S3Class}:}{Object of class
  \code{"character"} ~~ } \item{\code{columns}}{Named
  \code{character} vector. The names are the column names,
  and the values are the required class of the column.}
  \item{\code{exclusive}}{Object of class \code{logical}.
  If \code{TRUE}, then the data frame cannot contain any
  columns other than those in \code{columns}}
  \item{\code{constraints}}{Object of class \code{list}
  containing \code{function} elements.  Each function in
  the list should take one argument, and return
  \code{logical}.} }
}

\section{Methods}{
  \describe{ \item{[<-}{\code{signature(x =
  "DataFramePlus")}: ... } \item{[[<-}{\code{signature(x =
  "DataFramePlus")}: ... } \item{$<-}{\code{signature(x =
  "DataFramePlus")}: ... }
  \item{initialize}{\code{signature(.Object =
  "DataFramePlus")}: ... } }
}

\section{Extends}{
  \describe{ \item{\code{data.frame}}{Directly.} }
}
\examples{
new("DataFramePlus", data.frame(a=1:10),
    columns=c(a="numeric"))
}
\keyword{classes}

