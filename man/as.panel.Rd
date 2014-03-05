\name{as.panel}
\alias{as.panel}
\title{Quick panel data declaration}
\usage{
as.panel(data, panel, time, format = NA, date = NA, quiet = FALSE,
  detect = FALSE, ...)
}
\arguments{
  \item{data}{a data frame}

  \item{panel}{the name of the panel variable}

  \item{time}{the name of the time variable}

  \item{format}{the format of the panel variable, e.g. for
  country-level data, "iso3n" or any other country code
  supported by the \code{\link{countrycode}} package; will
  default to the generic "name" format if missing}

  \item{date}{the format of the time variable, in any
  notation supported by the \code{\link{parse_date_time}}
  function of the \code{\link{lubridate}} package; will
  automatically detect variables}

  \item{detect}{autodetect country and year formats;
  defaults to \code{FALSE}}

  \item{...}{any other method passed to
  \code{\link{as.psData}}}
}
\description{
Declares a data frame as a \code{\link{psData-class}}
object. See \code{\link{as.psData}} for details on formats
and additional arguments.
}
\examples{
# Load Reinhart and Rogoff demo data.
data(debt)
# Convert to "country name-year" panel format.
as.panel(debt, "Country", "Year")
}
\seealso{
\code{\link[lubridate]{parse_date_time}}
}

