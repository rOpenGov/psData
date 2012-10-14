\name{validate_data_frame}
\alias{validate_data_frame}
\title{Check column names and classes of a \code{data.frame}}
\usage{
  validate_data_frame(object, columns, exclusive = FALSE,
    keys = character(), constraints = list())
}
\arguments{
  \item{object}{\code{data.frame} to be validated.}

  \item{columns}{Named \code{character} vector. Names are
  required}

  \item{exclusive}{\code{logical} If \code{TRUE}, then
  \code{object} cannot contain any columns other than those
  in \code{columns} columns in \code{x}, values are the
  classes of those columns.}

  \item{keys}{\code{character} Names of columns which
  should be jointly unique.}

  \item{constraints}{\code{list} of functions. Each
  function should take only one argument, and return
  \code{logical}.}
}
\value{
  If valid, then \code{TRUE}, else \code{character} with an
  error message.
}
\description{
  Check column names and classes of a \code{data.frame}
}

