\name{get_qogbook}
\alias{get_qogbook}
\title{Download Quality of Government codebooks}
\usage{
get_qogbook(file = FALSE, version = "std", path = "", replace = FALSE)
}
\arguments{
  \item{file}{a filename to save the codebook at. If set to
  \code{TRUE}, the name of the codebook on the QOG server
  will be used. If set to \code{FALSE} (the default), the
  function only returns the link to the dataset. The
  filename must end in \code{.pdf}.}

  \item{replace}{whether to download the dataset even if a
  file already exists at the download location. Defaults to
  \code{FALSE}.}

  \item{path}{a folder path to append to the filename.}

  \item{version}{the QOG version: \code{std} (Standard),
  \code{soc} (Social Policy), \code{bas} (Basic) or
  \code{exp} (Expert). Defaults to \code{std}.}
}
\description{
Function to download Quality of Government (QOG) codebooks.
Please visit the QOG Institute website at
\url{http://www.qog.pol.gu.se/} for a presentation of QOG
research.
}
\details{
The function mimics Richard Svensson's \code{qogbook} Stata
command.
}
\examples{
# Show the URL to the QOG Standard dataset codebook.
get_qogbook()
## Download QOG Standard codebook with default filename (not run).
# get_qogbook(file = TRUE)
## Download QOG Basic dataset codebook to specific filename (not run).
# get_qogbook(file = "qog.basic.codebook.pdf", version = "bas")
}
\author{
Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
\code{\link{rQog}}
}
\keyword{codebooks}
\keyword{qog,}

