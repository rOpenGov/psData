\name{panel_shift}
\alias{panel_lag}
\alias{panel_lead}
\alias{panel_shift}
\title{Lag or lead a panel data variable}
\usage{
panel_shift(data, variable, k = 1)
}
\arguments{
  \item{data}{a data frame carrying an \code{\link{psData}}
  attribute.}

  \item{variable}{the variable to lag/lead.}

  \item{k}{the number of lags/leads.}
}
\description{
Function to lag/lead a \code{\link{psData}} object. Based
on the \code{shift} function by TszKin Julian Chan.
}
\details{
You might want to use the \code{panel_lag} and
\code{panel_lead} convenience wrappers: \itemize{ \item
\code{panel_lag} will perform a negative shift of \code{k}
lags \item \code{panel_lag} will perform a positive shift
of \code{k} leads }
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
# Subset to a short series.
QOG = panel_subset(qog.ts.demo, year \%in\% 2008:2012 & cname == "United States")
# Lag by one time period.
QOG$L1.wdi_gdpc = panel_lag(QOG, "wdi_gdpc", -1)
# Lead by two time periods.
QOG$F2.wdi_gdpc = panel_lead(QOG, "wdi_gdpc", 2)
# Check results.
QOG[, c("year", "wdi_gdpc", "L1.wdi_gdpc", "F2.wdi_gdpc")]
# Full method.
cbind(QOG, sapply(-2:2, panel_shift, data = QOG, variable = "wdi_hec"))
}
\references{
Christopher Gandrud, "Slide: one function for lag/lead
variables in data frames, including time-series
cross-sectional data",
\url{http://christophergandrud.blogspot.com/2013/05/slide-one-function-for-laglead.html}
}
\seealso{
\code{\link{psData}}
}
\keyword{panel}
\keyword{ts}

