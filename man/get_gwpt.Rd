\name{get_gwpt}
\alias{get_gwpt}
\title{Get state-level historical events from Gledistch & Ward and Powell & Thyne}
\usage{
get_gwpt(start = 1945, end = 2013, independence = "gw_indep",
  coups = "pt_coup")
}
\arguments{
  \item{start}{the first year of measurement to include.
  Defaults to \code{1945}.}

  \item{end}{the last year of measurement to include.
  Defaults to \code{2013}.}

  \item{independence}{name under which to create the state
  independence variable. Defaults to \code{"gw_indep"}. See
  'Details'.}

  \item{coups}{name under which to create the state coups
  variable. Defaults to \code{"pt_coup"}. See 'Details'.}
}
\value{
a data frame with country-year observations
}
\description{
Function to download state-level historical events from
Gleditsch and Ward (1999, updated 3 May 2013) and Powell
and Thyne (2011, updated c. 2013). The result is a
\code{\link{class-psData}} object. There is a small bug
with the slots that returns two warnings.
}
\details{
The variables produced by this function are \bold{gw_indep}
(years of independence, coded 0/1), from Gleditsch and
Ward, and \bold{pt_coup} (attempted and successful
\emph{coups d'\'{E}tat}), from Powell and Thyne. The
revised gross domestic product and population estimates
from Gleditsch (2002) are based on older and shorter
versions of the Penn World Table than the QOG datasets, and
are therefore not included.
}
\examples{
# Download data up to 2012.
head(G <- get_gwpt(end = 2012))
if(require(countrycode) & require(ggplot2)) {
  # Get geographic markers.
  G$iso3c = countrycode(G$ccode, "iso3n", "iso3c")
  G$continent = countrycode(G$ccode, "iso3n", "continent")
  # Plot the full data.
  qplot(data = subset(G, !is.na(continent)),
            x = year, y = reorder(iso3c, as.numeric(pt_coup), mean),
            fill = continent, alpha = pt_coup, geom = "tile") +
    scale_fill_brewer("Continent", palette = "Set1") +
    scale_alpha_manual("Event", values = c(0, .5, 1)) +
    scale_x_continuous(breaks = seq(1953, 2013, 10)) +
    labs(y = NULL) +
    theme(axis.text.y = element_text(size = 8))
}
if(require(ggplot2)) {
  # Time distribution.
  qplot(data = subset(G, pt_coup != "No verified coup attempt"),
        x = year, group = pt_coup, color = pt_coup, binwidth = 3, alpha = I(2/3),
        stat = "bin", geom = "line") +
    theme(legend.position = "bottom") +
    scale_fill_brewer("", palette = "Set1") +
    labs(x = NULL)
}
}
\author{
Francois Briatte \email{f.briatte@ed.ac.uk}
}
\references{
Gleditsch, Kristian S. & Michael D. Ward. 1999. "Interstate
System Membership: A Revised List of the Independent States
since 1816.". \emph{International Interactions} 25:
393-413,
\url{http://privatewww.essex.ac.uk/~ksg/statelist.html}.

Gleditsch, Kristian S. 2002. "Expanded Trade and GDP Data,"
\emph{Journal of Conflict Resolution} 46: 712-24.

Powell, Jonathan M. & Clayton L. Thyne. 2011. "Global
Instances of Coups from 1950 to 2010: A New Dataset.".
\emph{Journal of Peace Research} 48(2): 249-259,
\url{http://www.uky.edu/~clthyn2/coup_data/home.htm}.
}

