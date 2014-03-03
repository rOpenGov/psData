\name{get}
\alias{get}
\title{Data getters}
\usage{
get(x, ...)
}
\arguments{
  \item{url}{the URL of the data (modify if you need
  another version of the dataset)}

  \item{read}{the format of the data (e.g. \code{csv},
  \code{dta}, \code{spss})}

  \item{var.n}{the panel variable}

  \item{var.t}{the time variable}

  \item{...}{additional arguments passed to
  \code{\link{get_data}}}
}
\description{
\itemize{ \item Polity IV \item DPI }
}
\details{
Override the default parameters only if you know exactly
what alternative version and format of the data you need.
}
\seealso{
\code{\link{get_data}}, \code{\link{get_polity4}},
\code{\link{get_dpi}}
}

