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
# Load Reinhart and Rogoff demo data.
data(debt)
# Convert to "country name-year" panel format.
debt = as.panel(debt, "Country", "Year")
# Lag real GDP growth by one time period.
debt$L1.growth = panel_lag(debt, "growth", -1)
# Lead real GDP growth by two time periods.
debt$F2.growth = panel_lead(debt, "growth", 2)
# Show results.
head(debt[, c("Year", "growth", "L1.growth", "F2.growth")])
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

