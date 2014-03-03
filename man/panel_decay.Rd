\name{panel_decay}
\alias{panel_decay}
\title{Linear decay}
\source{
Zachary M. Jones, "Some Time-Series Functions for Panels
with Missingness",
\url{http://www.zmjones.com/panel-ts.html}
}
\usage{
panel_decay(data, x, cutpoint)
}
\arguments{
  \item{data}{a data frame with the \code{\link{psData}}
  attribute.}

  \item{x}{the variable for which to compute linear decay}

  \item{cutpoint}{the decay cut-point.}
}
\description{
Linear decay function adapted from the
\code{\link[doBy]{doBy}} package by Zachary M. Jones, and
modified to understand the \code{\link{psData}} attribute.
}
\author{
Zachary M. Jones
}
\seealso{
\code{\link[doBy]{doBy}}
}
\keyword{panel}
\keyword{ts}

