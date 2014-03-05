\name{try_countrycode}
\alias{try_countrycode}
\title{Autodetect a country code}
\usage{
try_countrycode(x, format = "iso3n")
}
\value{
the fraction of perfect matches
}
\description{
Looks for perfect matches between the values of a variable
and its converted values from a list of country codes
handled by the \code{\link{countrycode}} package. The codes
are assessed against their ISO-3C translation.
}
\keyword{internal}

