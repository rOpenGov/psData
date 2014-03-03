\name{get_qog}
\alias{get_qog}
\title{Get Quality of Government datasets in \code{xtdata} format}
\usage{
get_qog(file = FALSE, replace = FALSE, codebook = FALSE, path = "",
  version = "std", format = "cs", variables = NULL, years = NULL,
  date = "15may13", ...)
}
\arguments{
  \item{file}{a filename to save the dataset at. If set to
  \code{FALSE} (the default), the function just returns the
  link to the dataset. If set to \code{TRUE}, the server
  filename of the dataset is used, which returns either a
  CSV file if \code{version} is set to \code{std}, or a
  Stata \code{dta} file otherwise. See 'Details'.}

  \item{replace}{whether to overwrite the dataset even if a
  file already exists at the download location. Defaults to
  \code{FALSE}.}

  \item{path}{a folder path to prepend to the filename and
  to the codebook if \code{codebook} is not \code{FALSE}.}

  \item{version}{the QOG version: \code{std} (Standard),
  \code{soc} (Social Policy), \code{bas} (Basic) or
  \code{exp} (Expert). Defaults to \code{std}. See
  'Details'.}

  \item{format}{the QOG format, usually \code{cs} for
  cross-sectional data or \code{ts} for time series in the
  \code{std} and \code{bas} versions. See 'Details' for the
  full list of specifications. Defaults to \code{cs}.}

  \item{codebook}{whether to download the codebook. Calls
  \code{get_qogbook} by passing the \code{codebook},
  \code{version} and \code{path} arguments to it, where
  \code{codebook} is treated as the filename for the
  codebook. Defaults to \code{FALSE}.}

  \item{variables}{a selection of variables to import.
  \code{ccode} ISO-3N country codes \code{ccode} and
  \code{year} identifiers will be forced into the output if
  relevant.}

  \item{years}{a selection of years to import. Effective
  only with the \code{ts}, \code{tsl} or \code{ind}
  formats.}

  \item{...}{other arguments supplied to the import method,
  which is \code{read.csv} by default, or
  \code{\link[foreign]{read.dta}} if \code{file} is a Stata
  \code{dta} dataset, or \code{\link[foreign]{read.spss}}
  if \code{file} is a SPSS \code{sav} dataset.}
}
\description{
Function to download Quality of Government (QOG) data and
load it as a data frame in R. The result carries an
\code{\link{xtdata}} attribute that can be passed to the
\code{\link{xtmerge}} panel data method. Please visit the
QOG Institute website at \url{http://www.qog.pol.gu.se/}
for a presentation of QOG research.
}
\details{
This version of the package handles all four QOG datasets:
\tabular{lcl}{ QOG Standard \tab \code{std} \tab 15 May
2013\cr QOG Social Policy \tab \code{soc} \tab 4 April
2012\cr QOG Basic \tab \code{bas}): \tab 28 March 2011\cr
QOG Expert Survey \tab \code{exp} \tab 3-6 September
2012\cr URL: \tab \tab \url{http://www.qog.pol.gu.se}\cr }

Each QOG dataset is available in a variety of data formats:

\itemize{ \item QOG datasets \code{std} and \code{bas}
require format \code{cs} (cross-section) or \code{ts} (time
series).  \item QOG dataset \code{soc} requires format
\code{cs}, \code{tsl} (time series, long) or \code{tsw}
(time series, wide) \item QOG dataset \code{exp} requires
format \code{cntry} (country-level) or \code{ind}
(individual survey) }

The QOG Standard series comes in CSV, SPSS and Stata file
formats, CVS being the only format that contains numeric
codes instead of QOG value labels. Datasets outside of the
QOG Standard series are available only as Stata items and
require that \code{file} ends in \code{.dta} when
\code{version} is not \code{std}. The only exception is
dataset \code{csyom}, which automatically sets
\code{version} to \code{std} and requires \code{file} to
end in \code{.csv}. Filenames with inadequate extensions
will be modified to conform to these expectations if they
do not.
}
\examples{
# Show URL to QOG Standard cross-section.
get_qog()
# Show URL to QOG Social Policy time series, long format.
get_qog(version = "soc", format = "tsl")
## Download codebook and recent years from QOG Basic cross-section (not run).
# QOG = get_qog(file = "qog.cs.txt", version = "bas", format = "cs",
#         years = 2002:2012, codebook = TRUE)
## Download QOG Standard cross-section years of measurement (not run).
# QOG = get_qog(tempfile(fileext = ".csv"), format = "csyom")
## Show QOG years of measurement for Gini coefficient (not run).
# table(QOG$wdi_gini)
}
\author{
Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
\code{\link{get_qogbook}}, \code{\link[foreign]{read.dta}},
\code{\link[foreign]{read.spss}}
}
\keyword{csts}
\keyword{data}
\keyword{dpi}
\keyword{polity}
\keyword{qog}
\keyword{wdi}

