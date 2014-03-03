#' Get state-level historical events from Gledistch & Ward and Powell & Thyne
#'
#' Function to download state-level historical events from Gleditsch and Ward (1999, updated 3 May 2013) and Powell and Thyne (2011, updated c. 2013). 
#' The result is a \code{\link{class-psData}} object. There is a small bug with the slots that returns two warnings.
#'
#' @param start the first year of measurement to include. Defaults to \code{1945}.
#' @param end the last year of measurement to include. Defaults to \code{2013}.
#' @param independence name under which to create the state independence variable.
#' Defaults to \code{"gw_indep"}. See 'Details'.
#' @param coups name under which to create the state coups variable. 
#' Defaults to \code{"pt_coup"}. See 'Details'.
#' @details The variables produced by this function are \bold{gw_indep} (years of independence, coded 0/1), from Gleditsch and Ward, and \bold{pt_coup} (attempted and successful \emph{coups d'\'{E}tat}), from Powell and Thyne. The revised gross domestic product and population estimates from Gleditsch (2002) are based on older and shorter versions of the Penn World Table than the QOG datasets, and are therefore not included.
#' @return a data frame with country-year observations
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @references Gleditsch, Kristian S. & Michael D. Ward. 1999. "Interstate 
#' System Membership: A Revised List of the Independent States since 1816.". 
#' \emph{International Interactions} 25: 393-413, 
#' \url{http://privatewww.essex.ac.uk/~ksg/statelist.html}.
#' 
#' Gleditsch, Kristian S. 2002. "Expanded Trade and GDP Data," 
#' \emph{Journal of Conflict Resolution} 46: 712-24.
#' 
#' Powell, Jonathan M. & Clayton L. Thyne. 2011.
#' "Global Instances of Coups from 1950 to 2010: A New Dataset.". 
#' \emph{Journal of Peace Research} 48(2): 249-259, 
#' \url{http://www.uky.edu/~clthyn2/coup_data/home.htm}.
#' @examples
#' # Download data up to 2012.
#' head(G <- get_gwpt(end = 2012))
#' if(require(countrycode) & require(ggplot2)) {
#'   # Get geographic markers.
#'   G$iso3c = countrycode(G$ccode, "iso3n", "iso3c")
#'   G$continent = countrycode(G$ccode, "iso3n", "continent")
#'   # Plot the full data.
#'   qplot(data = subset(G, !is.na(continent)),
#'             x = year, y = reorder(iso3c, as.numeric(pt_coup), mean),
#'             fill = continent, alpha = pt_coup, geom = "tile") + 
#'     scale_fill_brewer("Continent", palette = "Set1") +
#'     scale_alpha_manual("Event", values = c(0, .5, 1)) +
#'     scale_x_continuous(breaks = seq(1953, 2013, 10)) +
#'     labs(y = NULL) +
#'     theme(axis.text.y = element_text(size = 8))
#' }
#' if(require(ggplot2)) {
#'   # Time distribution.
#'   qplot(data = subset(G, pt_coup != "No verified coup attempt"), 
#'         x = year, group = pt_coup, color = pt_coup, binwidth = 3, alpha = I(2/3),
#'         stat = "bin", geom = "line") +
#'     theme(legend.position = "bottom") +
#'     scale_fill_brewer("", palette = "Set1") +
#'     labs(x = NULL)
#' }
#' @export
#' @keywords gleditsch ward powell thyne data csts
get_gwpt = function(start = 1945, end = 2013, independence = "gw_indep", coups = "pt_coup") {
  message("Downloading data for years ", start, "-", end, "...")
  
  # Gleditsch and Ward independence data
  url = "http://privatewww.essex.ac.uk/~ksg/data/iisystem.dat"
  x = read.table(url, quote = "", sep = "\t")[, -2:-3]
  names(x) = c("ccode", "start", "end")
  
  x$start = as.numeric(substring(x$start, 7))
  x$end = as.numeric(substring(x$end, 7))
  # drop left of timeline
  x = x[x$end > start, ]
  # right censor
  x[x$end >= end, ] = end
  
  # Powell and Thyne coup attempts data
  url = "http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt"
  y = read.csv(url, sep = "\t")[, -4:-5]
  # rename variables
  names(y) = c("country.name", "ccode", "year", coups)
  
  y$country.name[y$country.name == "Guinea Bissau"] = "Guinea-Bissau"
  
  d = expand.grid(unique(y[, "ccode"]), seq(start, end))
  names(d) = c("ccode", "year")
  d = d[order(d[, "ccode"]), ]
  
  for(i in 1:nrow(d)) {
    m = which(x[, "ccode"] == d[i, "ccode"])
    if(length(m) > 0)
      d[i, independence] = d[i, "year"] %in% seq(x$start[m], x$end[m])
  }
  d[, independence] = as.numeric(d[, independence])
  
  # add coups d'Etat
  d[, coups] = 0
  for(i in 1:nrow(y)) {
    m = which(d[, "ccode"] == y[i, "ccode"] & d[, "year"] == y[i, "year"])
    if(length(m) > 0)
      d[m, coups] = y[i, coups]
    else
      message("Excluding ", y[i, "country.name"], " ", y[i, "year"], " (out of time period)")
  }
  d[, coups] = factor(d[, coups], labels = c(
    "No verified coup attempt", 
    "Unsuccessful coup attempt", 
    "Successful coup attempt"))
  
  d = merge(unique(y[, 1:2]), d, by = "ccode", all.y = TRUE)
  # G+W country codes
  d[, "ccodegw"] = d[, "ccode"]
  d[, "ccode"] = countrycode(d[, "country.name"], "country.name", "iso3n")
  # historical states
  m = which(d[, "country.name"] == "Yemen Arab Republic; N. Yemen")
  d[m, "ccode"] = 886
  m = which(d[, "country.name"] == "Yemen People's Republic; S. Yemen")
  d[m, "ccode"] = 720
  d[, "country.name"] = NULL
  return(psData(d,
                design = list(
                  panel = c("ccode", "ccodegw"),
                  format = c(ccode = "iso3n", ccodegw = "ccodegw"),
                  time = c("year"),
                  date = c(year = "%Y")),
                meta = list(
                  name = "Coups and independence state-level data",
                  meta = c(aut = "Gleditsch and Ward", aut = "Powell and Thyne"),
                  date = "",
                  url = "http://www.uky.edu/~clthyn2/coup_data/"
                )))
}
