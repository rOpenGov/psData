\name{DropNA.psData}
\alias{DropNA.psData}
\title{Drop rows from a data frame with missing values in the OutCountryID variable.}
\source{
Largely based on \code{DropNA} from the \code{DataCombine}
package.
}
\usage{
DropNA.psData(data, Var)
}
\arguments{
  \item{data}{a data frame object.}

  \item{Var}{a character vector naming the variables you
  would like to have only non-missing (NA) values.}
}
\description{
Drop rows from a data frame with missing values in the
OutCountryID variable.
}
\keyword{internals}

