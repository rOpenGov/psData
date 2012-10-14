\name{subclass_data_frame_plus}
\alias{subclass_data_frame_plus}
\title{Create subclasss of \code{DataFramePlus}}
\usage{
  subclass_data_frame_plus(class, columns = character(),
    exclusive = FALSE, keys = character(),
    where = topenv(parent.frame()), constraints = list())
}
\arguments{
  \item{class}{\code{character} Name of the new class.}

  \item{columns}{Named \code{character} vector. The names
  are the names of required columns; the values are the
  classes of those columns.}

  \item{exclusive}{\code{logical} If \code{TRUE}, then the
  data frame can only contain the columns in
  \code{columns}.}

  \item{keys}{\code{character} Columns in the data fram
  which a jointly unique.}

  \item{constraints}{\code{list} of functions. Each
  function should take only one argument, and return
  \code{logical}.}

  \item{where}{\code{environment}. The environment in which
  to store the definition. See \code{\link{setClass}}.}
}
\value{
  Invisibly returns a constructor function for the new
  class.
}
\description{
  This function creates a class which directly extends
  \code{DataFramePlus} with the requirement that the slots
  (\code{required}, \code{classes}, \code{exclusive}, and
  \code{keys}) take specific values.
}
\examples{
foo <- subclass_data_frame_plus("foo", columns=c(a="numeric"))
foo(data.frame(a=1:10))
try(foo(data.frame(b=1:10)))
}

