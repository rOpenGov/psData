\name{get_polity4}
\alias{get_polity4}
\title{Downloads Polity IV}
\usage{
get_polity4(url = "http://www.systemicpeace.org/inscr/p4v2012.sav",
  var.n = "country", var.t = "year", read = "spss", ...)
}
\arguments{
  \item{url,}{read, var.n, var.t, ... \code{\link{get}}
  parameters}
}
\value{
a data frame
}
\description{
Downloads the Polity IV data set. It keeps specified
variables and creates a standard country ID variable that
can be used for merging the data with other data sets.
}
\examples{
\dontrun{
# Download full data set
PolityData <- get_polity4()

# Create data frame with only the main Polity democracy variable (polity2)
Polity2Data <- get_polity(vars = 'polity2', OutCountryID = 'imf')
}
}
\seealso{
\code{\link{countrycode}}, \code{\link{CountryID}},
\code{\link{WinsetCreator}}
}

