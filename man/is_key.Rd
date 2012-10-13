\name{is_key}
\alias{is_key}
\title{Check whether columns could be a key to a data.frame}
\usage{
  is_key(x, i)
}
\arguments{
  \item{x}{\code{data.frame} to check}

  \item{i}{\code{character} or \code{integer} Column names}
}
\value{
  \code{logical}. \code{TRUE} if no duplicated values of
  \code{i} in \code{x}.
}
\description{
  Check whether columns could be a key to a data.frame
}

