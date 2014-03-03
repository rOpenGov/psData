\name{get_data}
\alias{get_data}
\title{Get data in all formats read by the default packages}
\usage{
get_data(url = NULL, var.n = "country", var.t = "year", read = "csv",
  vars = NULL, OutCountryID = "iso2c", standardCountryName = TRUE,
  na.rm = TRUE, duplicates = "message", fromLast = FALSE)
}
\arguments{
  \item{url}{character string. The URL for the data set you
  would like to download.}

  \item{var.n}{the main panel variable}

  \item{var.t}{the main time variable}

  \item{read}{the format of the data (\code{csv},
  \code{dta}, \code{spss} or any other \code{read.method}
  function).}

  \item{vars}{character vector containing the variables to
  keep. If \code{vars = NULL} then the entire data set is
  returned. Note that the \code{country} and \code{year}
  variables are always returned.}

  \item{OutCountryID}{character string. The type of country
  ID you would like to include in the output file along
  with the country name. See \code{\link{countrycode}} for
  available options.}

  \item{standardCountryName}{logical. Whether or not to
  standardise the country names variable based on
  \code{country.name} from \code{\link{countrycode}}.}

  \item{na.rm}{logical. Drop observations where
  \code{OutCountryID} is \code{NA}.}

  \item{duplicates}{character specifying how to handle
  duplicated country-year observations. Can be set to
  \code{none} to do nothing, \code{message} to simply
  report duplicates, \code{drop} to report and drop
  duplicates, and \code{return} to return a data frame with
  only duplicated observations (see also \code{fromLast}).}

  \item{fromLast}{logical indicating if duplication should
  be considered from the reverse side. Only relevant if
  \code{duplicates = 'drop'} or \code{duplicates = 'out'}.}
}
\value{
a data frame
}
\description{
The get_data function requires a URL, a read method ('csv'
by default), a panel variable and a time variable. These
parameters are easy to pass through little functions like
the other get methods in this file.
}

