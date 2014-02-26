\name{IMFDreherGet}
\alias{IMFDreherGet}
\title{Downloads Dreher's data set of IMF programs and World Bank projects (1970-2011)}
\source{
Data website:
\url{http://www.uni-heidelberg.de/fakultaeten/wiso/awi/professuren/intwipol/datasets_en.html}.

When using the IMF data, please cite: Dreher, Axel, 2006,
IMF and Economic Growth: The Effects of Programs, Loans,
and Compliance with Conditionality, World Development 34,
5: 769-788.

When using the World Bank data, please cite: Boockmann,
Bernhard and Axel Dreher, 2003, The Contribution of the IMF
and the World Bank to Economic Freedom, European Journal of
Political Economy 19, 3: 633-649.
}
\usage{
IMFDreherGet(url = "http://axel-dreher.de/Dreher\%20IMF\%20and\%20WB.xls",
  sheets = c("WB other agreed", "WB environment agreed"),
  OutCountryID = "iso2c", message = TRUE, standardCountryName = TRUE)
}
\arguments{
  \item{url}{character string. The URL for the Dreher data
  set you would like to download. Note: it must be for the
  xlx version of the file. Currently only the 1970-2011
  version is supported.}

  \item{sheets}{character vector of the Excel sheets
  (variables) that you would like to return. See Details
  for more information.}

  \item{OutCountryID}{character string. The type of country
  ID you would like to include in the output file along
  with the country name. See \code{\link{countrycode}} for
  available options.}

  \item{standardCountryName}{logical. Whether or not to
  standardise the country names variable based on
  \code{country.name} from \code{\link{countrycode}}.#'}

  \item{message}{logical. Whether or not to notify you
  which of sheets are being cleaned and organised.}
}
\value{
a data frame
}
\description{
Downloads Dreher's data set of IMF programs and World Bank
projects (1970-2011)
}

