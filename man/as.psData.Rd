\name{as.psData}
\alias{as.psData}
\title{as.psData}
\usage{
as.psData(data, design = list(), meta = list(), ...)
}
\arguments{
  \item{data}{}
}
\description{
Converts a data frame to \code{\link{class-psData}} by
adding two lists of attributes to the data: \itemize{ \item
\code{design}. define panel attributes like country codes
and time units \item \code{meta}. define a minimal set of
references to describe the data (optional) }
}
\details{
The \code{class-psData} also optionally supports all
methods from the \code{DataFrameConstr} package: \itemize{
\item \code{columns}. protect the classes of selected
columns, as in \code{columns = c(a = "numeric", b = "ANY",
c = "factor")} \item \code{constraints}. force selected
variables to follow logical conditions, as in
\code{constraints = list(function(x) { x$a > 0 }} \item
\code{exclusive}. protect the number of rows (\code{FALSE}
by default) }
}
\examples{
# Load Reinhart and Rogoff demo data.
data(debt)
# Add full panel details and metadata.
debt = as.psData(debt,
          design = list(
            # data design
            panel = c("Country"),
            format = c(Country = "name"),
            time = c("Year"),
            date = c(Year = "\%Y")
          ),
          meta = list(
            # descriptive
            name = "Reinhart and Rogoff data, edited from Herndon et al.'s script.",
            # similar to R packages
            author = "Cosma Shalizi",
            # versioning
            date = "2013-04-29",
            version = "",
            url = "http://www.stat.cmu.edu/~cshalizi/uADA/13/hw/11/debt.csv"
          ))
# Standard data frame.
head(data.frame(debt))
# psData data frame.
debt
}
\references{
\code{DataFrameConstr}, by Jeffrey Arnold:
\url{https://github.com/jrnold/DataFrameConstr}
}
\seealso{
\code{\link{as.panel}}
}

