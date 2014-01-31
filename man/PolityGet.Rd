\name{PolityGet}
\alias{PolityGet}
\title{Downloads Polity IV}
\usage{
PolityGet(url = "http://www.systemicpeace.org/inscr/p4v2012.sav",
  vars = NULL, OutCountryID = "iso2c", na.rm = TRUE)
}
\arguments{
  \item{url}{character string. The URL for the Polity IV
  data set you would like to download. Note: it must be for
  the SPSS version of the file.}

  \item{vars}{character vector containing the variables to
  keep. If \code{vars = NULL} then the entire data set is
  returned. Note that the \code{country} and \code{year}
  variables are always returned.}

  \item{OutCountryID}{character string. The type of country
  ID you would like to include in the output file along
  with the country name. See \code{\link{countrycode}} for
  available options.}

  \item{na.rm}{logical. Drop observations where
  \code{OutCountryID} is \code{NA}.}
}
\description{
Downloads the Polity IV data set. It keeps specified
variables and creates a standard country ID variable that
can be used for merging the data with other data sets.
}
\examples{
# Download full data set
PolityData <- PolityGet()

# Create data frame with only the main Polity democracy variable (polity2)
Polity2Data <- PolityGet(vars = 'polity2',
                         OutCountryID = 'imf')
}
\seealso{
\code{\link{countrycode}}
}

