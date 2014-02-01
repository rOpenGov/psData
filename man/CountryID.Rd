\name{CountryID}
\alias{CountryID}
\title{Function for creating standardised country names and ID variables}
\usage{
CountryID(data, countryVar = "country", OutCountryID = "iso2c",
  standardCountryName = TRUE, duplicates = "message", timeVar = NULL,
  fromLast = FALSE)
}
\arguments{
  \item{data}{a data frame object}

  \item{countryVar}{character string naming the
  country.name variable. See \code{\link{countrycode}}.}

  \item{OutCountryID}{character string. The type of country
  ID you would like to include in the output file along
  with the country name. See \code{\link{countrycode}} for
  available options.}

  \item{standardCountryName}{logical. Whether or not to
  standardise the country names variable based on
  \code{country.name} from \code{\link{countrycode}}.}

  \item{duplicates}{character specifying how to handle
  duplicated country or country-time observations (for the
  latter see \code{timeVar}). Can be set to \code{none} to
  do nothing, \code{message} to simply report duplicates,
  \code{drop} to report and drop duplicates, and
  \code{return} to return a data frame with only duplicated
  observations (see also \code{fromLast}).}

  \item{timeVar}{character string indicating the name of a
  time variable. For example, country time series often
  have separate rows based on a \code{year} variable. This
  is used solely to determine if there are duplicated
  country-time values.}

  \item{fromLast}{logical indicating if duplication should
  be considered from the reverse side. Only relevant if
  \code{duplicates = 'drop'} or \code{duplicates = 'out'}.}
}
\description{
Function for creating standardised country names and ID
variables based on capabilities from the
\code{\link{countrycode}} package. The function also
reports if duplicated country IDs have been created and
lets the user either drop these or return only duplicated
values for inspection.
}
\seealso{
{\code{\link{duplicated}}}
}

