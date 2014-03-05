\name{panel_sample}
\alias{panel_sample}
\title{Sample out of a \code{\link{psData-class}} data frame}
\usage{
panel_sample(data, n = 20)
}
\arguments{
  \item{data}{\code{\link{psData-class}} data frame}

  \item{n}{how many observations to sample}
}
\description{
Function to extract a sample of observations out of a panel
dataset, preserving all time measurements for each sampled
observation.
}
\examples{
# Load Reinhart and Rogoff demo data.
data(debt)
# Convert to "country name-year" panel format.
debt = as.panel(debt, "Country", "Year")
# Random sample of five countries.
unique(panel_sample(debt, 5)$Country)
# Random cross-section of year 2000.
panel_subset(panel_sample(debt, 10), Year == 2000)
}
\seealso{
\code{\link{sample}}
}
\keyword{panel}

