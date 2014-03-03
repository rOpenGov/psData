\name{get_data}
\alias{get_data}
\title{Get data in all formats read by the default packages}
\usage{
get_data(url = NULL, var.n = "country", var.t = "year", read = "csv",
  vars = NULL, OutCountryID = "iso2c", standardCountryName = TRUE,
  na.rm = TRUE, duplicates = "message", fromLast = FALSE, args = NULL)
}
\arguments{
  \item{url}{character string; the URL for the dataset you
  would like to download; automatically set by other
  \code{get} methods (see 'Details').}

  \item{var.n}{character string; the main panel variable
  (column name). Defaults to \code{"country"};
  automatically set by other \code{get} methods (see
  'Details').}

  \item{var.t}{character string; the main time variable
  (column name). Defaults to \code{"year"}; automatically
  set by other \code{get} methods (see 'Details').}

  \item{read}{character string; the format of the data
  (\code{csv}, \code{dta}, \code{spss}, \code{table} or any
  other supported \code{read.method} function). Defaults to
  \code{"csv"}; automatically set by other \code{get}
  methods (see 'Details').}

  \item{vars}{character vector containing the variables to
  keep. If \code{vars = NULL} then the entire data set is
  returned. Note that the main panel and time variables are
  always returned.}

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

  \item{args}{additional argument to the \code{read}
  function. Automatically sets SPSS files to import
  correctly.}
}
\value{
a data frame, possibly of \code{\link{class-psData}}
(expected output)
}
\description{
The get_data function requires a URL, a read method ('csv'
by default), a panel variable and a time variable. These
parameters are easy to pass through little functions like
the other get methods in this file.
}
\details{
The \code{get} method scrapes an online locator
(\code{url}), reads it into a recognized data format
(\code{read}), and requires its main panel (\code{var.n})
and time (\code{var.t}) to be imported. The \code{psData}
contains the following \code{get} presets: \itemize{ \item
\code{\link{get_polity4}}. Polity IV data \item
\code{\link{get_dpi}}. DPI data \item
\code{\link{get_qog}}. QOG data (untested, also produces
cross-section standard data frames) }
}
\keyword{data}
\keyword{download}
\keyword{get}

