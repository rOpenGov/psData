\name{validate_data_frame}
\alias{validate_data_frame}
\title{Validate \code{data.frame}: column names, classes, and arbitrary constraints}
\usage{
  validate_data_frame(object, columns = NULL,
    exclusive = FALSE, constraints = list())
}
\arguments{
  \item{object}{\code{data.frame} to be validated.}

  \item{columns}{Named \code{character} vector. Names are
  required}

  \item{exclusive}{\code{logical} If \code{TRUE}, then
  \code{object} cannot contain any columns other than those
  in \code{columns} columns in \code{x}, values are the
  classes of those columns.}

  \item{constraints}{\code{list} of functions. Each
  function should take only one argument, and return
  \code{logical}.}
}
\value{
  If valid, then \code{TRUE}, else \code{character} with an
  error message.
}
\description{
  Check that at \code{data.frame} has columns of specified
  classtypes, and satisfies arbitrary constraints.
}
\examples{
data(iris)
# check that the iris dataset has numeric column Sepal.Length
# and factor Species.
validate_data_frame(iris,
                    columns=c(Sepal.Length="numeric",
                              Species="factor"))
# Error because iris does not have column 'foo'
try(validate_data_frame(iris,
                    columns=c(foo="numeric")))
# Error because Sepal.Length is not an integer
try(validate_data_frame(iris,
                    columns=c(foo="ineger")))
}

