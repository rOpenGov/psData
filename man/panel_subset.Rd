\name{panel_subset}
\alias{panel_subset}
\title{Subset a \code{\link{psData}} object}
\usage{
panel_subset(data, formula, select = names(data), drop = FALSE)
}
\arguments{
  \item{data}{a data frame of \code{\link{psData-class}}.}

  \item{formula}{a logical formula to subset to.}

  \item{select}{the names of the variables to keep.}

  \item{drop}{passed on to \code{[} indexing operator.}

  \item{...}{other methods passed to \code{\link{subset}}}
}
\value{
a data frame
}
\description{
No specific behaviour for the moment, just checking that we
are hacking the \code{subset} method properly by
redirecting \code{psData} objects to here.
}
\details{
The evaluation method of the \code{eval} function call is
explained at
\url{https://github.com/hadley/devtools/wiki/Computing-on-the-language#non-standard-evaluation-in-subset}.
}
\examples{
# Load Reinhart and Rogoff demo data.
data(debt)
# Convert to "country name-year" panel format.
debt = as.panel(debt, "Country", "Year")
# Subset to negative growth country-years.
panel_subset(debt, growth < 0)
# Plot debt-to-growth by decade.
if(all(require(ggplot2), require(MASS), require(splines))) {
  library(splines)
  library(MASS)
  debt$Decade = 10 * debt$Year \%/\% 10
  qplot(data = debt, x = growth, y = ratio,
        group = Country, color = ratio < 90,
        alpha = I(.5)) +
    facet_wrap(~ Decade, scales = "free_x", nrow = 2) +
    geom_line(alpha = .25) +
    geom_smooth(aes(group = NULL, color = NULL),
                method ="rlm", formula = y ~ ns(x, 3),
                alpha = .25, fill = "lightblue") +
    scale_color_brewer("", palette = "Set1") +
    guides(color = FALSE) +
    labs(y = "debt/GDP ratio (ratios above 90\% in red)", x = "real GDP growth") +
    theme_bw(14)
}
}
\author{
Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
\code{\link{psData}}, \code{\link{subset}}
}
\keyword{panel}

