\name{CountryID}
\alias{CountryID}
\title{Internal function for creating standardised country names and ID variables}
\usage{
CountryID(data, countryVar = "country", OutCountryID = "iso2c")
}
\arguments{
  \item{data}{a data frame object}

  \item{countryVar}{character string naming the
  country.name variable. See \code{\link{countrycode}}.}

  \item{OutCountryID}{character string. The type of country
  ID you would like to include in the output file along
  with the country name. See \code{\link{countrycode}} for
  available options.}
}
\description{
Internal function for creating standardised country names
and ID variables
}
\keyword{internals}

