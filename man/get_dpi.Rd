\name{get_dpi}
\alias{get_dpi}
\title{Downloads the Database of Political Institutions (DPI)}
\usage{
get_dpi(url = "http://bit.ly/1jZ3nmM", var.n = "countryname",
  var.t = "year", read = "dta", ...)
}
\arguments{
  \item{url,}{read, var.n, var.t, ... \code{\link{get}}
  parameters}
}
\value{
a data frame
}
\description{
Downloads the Database of Political Institutions (DPI) data
set. It keeps specified variables and creates a standard
country ID variable that can be used for merging the data
with other data sets.
}
\details{
Note: a bit.ly URL is used to shorten the Stata formatted
data set's URL due to CRAN requirements.
}
\examples{
\dontrun{
# Download full data set
DpiData <- get_dpi()

# Create data frame with only the military variable
DpiSub <- get_dpi(vars = 'military', OutCountryID = 'imf')
}
}
\seealso{
\code{\link{countrycode}}, \code{\link{CountryID}},
\code{\link{WinsetCreator}}
}
\keyword{csts}
\keyword{data}
\keyword{dpi}

