\name{DpiGet}
\alias{DpiGet}
\title{Downloads the Database of Political Institutions (DPI)}
\usage{

  DpiGet(url = "http://siteresources.worldbank.org/INTRES/Resources/469232-1107449512766/DPI2012.dta",
  vars = NULL, OutCountryID = "iso2c", na.rm = TRUE)
}
\arguments{
  \item{url}{character string. The URL for the Polity IV
  data set you would like to download. Note: the link must
  be to a Stata formated file.}

  \item{vars}{character vector containing the variables to
  keep. If \code{vars = NULL} then the entire data set is
  returned. Note that \code{country} and \code{year}
  variables are always returned.}

  \item{OutCountryID}{character string. The type of country
  ID you would like to include in the output file along
  with the country name. See \code{\link{countrycode}} for
  available options.}

  \item{na.rm}{logical. Drop observations where
  \code{OutCountryID} is \code{NA}.}
}
\description{
Downloads the Database of Political Institutions (DPI) data
set. It keeps specified variables and creates a standard
country ID variable that can be used for merging the data
with other data sets.
}
\examples{
# Download full data set
DpiData <- DpiGet()

# Create data frame with only the military variable
DpiSub <- DpiGet(vars = 'military',
                         OutCountryID = 'imf')
}
\seealso{
\code{\link{countrycode}}
}

