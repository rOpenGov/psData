
#' Quick panel data declaration
#' 
#' Declares a data frame as a \code{\link{psData-class}} object. See 
#' \code{\link{as.psData}} for details on formats and additional arguments.
#' @param data a data frame
#' @param panel the name of the panel variable
#' @param time the name of the time variable
#' @param format the format of the panel variable, e.g. for country-level data,
#' "iso3n" or any other country code supported by the \code{\link{countrycode}} 
#' package; will default to the generic "name" format if missing
#' @param date the format of the time variable, in any notation supported by 
#' the \code{\link{parse_date_time}} function of the \code{\link{lubridate}} 
#' package; will automatically detect variables 
#' @param year autodetect year format; defaults to \code{TRUE}
#' @param ... any other method passed to \code{\link{as.psData}}
#' @seealso \code{\link[lubridate]{parse_date_time}}
#' @examples
#' # Load Reinhart and Rogoff demo data.
#' data(debt)
#' # Convert to "country name-year" panel format.
#' as.panel(debt, "Country", "Year")
#' @export
as.panel = function(data, panel, time, format = NA, date = NA, quiet = TRUE, ...) {

  # autodetection of years, months and dates with lubridate (vectorized)
  i = c("y", "m", "ymd")
  j = sapply(i, function(x) try_date(data[, time], date = x))
  j = names(j)[ which(j > .9) ]
  if(length(j) > 1) {
    date = j[1]
    if(!quiet)
      message(paste("Assigned date format", date, "to time variable", time, "\n",
                    "Discarded alternative formats:", 
                    paste0(j[-1], collapse = ", "), "\n "),
              paste0("Use as.panel('", panel, "', '", time, "', date = '...') to change."))
  } else if(length(j) == 1) {
    date = j
    if(!quiet)
      message(paste("Assigned date format", date, "to time variable", time, "\n "),
              paste0("Use as.panel('", panel, "', '", time, "', date = '...') to change."))
  } else {
    
    # unknown time
    if(is.na(date)) {
      date = "t"
      if(!quiet)
        message(paste("Assigned generic format 't' to time variable", time, "\n "),
                paste0("Use as.panel('", panel, "', '", time, "', date = '...') to change."))
    }

  }

  # autodetection of country codes with lubridate (vectorized)
  i = c("cowc", "cown", "iso3c", "iso3n", "iso2c", "imf", "fao", "un", "wb", "country.name")
  j = sapply(i, function(x) try_countrycode(data[, panel], format = x))
  j = names(j)[ which(j > .9) ]
  if(length(j) > 1) {
    format = j[1]
    if(!quiet)
      message(paste("Assigned country code format", format, "to panel variable", panel, "\n ",
                    "Discarded alternative formats:", 
                    paste0(j[-1], collapse = ", "), "\n"),
              paste0("Use as.panel('", panel, "', '", time, "', format = '...') to change."))
  } else if(length(j) == 1) {
    format = j
    if(!quiet)
      message(paste("Assigned country code format", format, "to panel variable", panel, "\n "),
              paste0("Use as.panel('", panel, "', '", time, "', format = '...') to change."))
  } else {
    
    # unknown panel format
    if(is.na(format)) {
      format = "name"
      if(!quiet)
        message(paste("Assigned generic format 'name' to panel variable", panel, "\n "),
                paste0("Use as.panel('", panel, "', '", time, "', format = '...') to change."))
    }
    
  }

  formats = format
  names(formats) = panel
  
  dates = date
  names(dates) = time
  
  return(as.psData(data, design = list("panel" = panel,
                                       "format" = formats, 
                                       "time" = time,
                                       "date" = dates), ...))
}

#' Lag or lead a panel data variable
#' 
#' Function to lag/lead a \code{\link{psData}} object. Based on the
#' \code{shift} function by TszKin Julian Chan.
#' 
#' You might want to use the \code{panel_lag} and \code{panel_lead} 
#' convenience wrappers:
#' \itemize{
#'   \item \code{panel_lag} will perform a negative shift of \code{k} lags
#'   \item \code{panel_lag} will perform a positive shift of \code{k} leads
#' }
#' 
#' @name panel_shift
#' @aliases panel_lag panel_lead
#' @export
#' @param data a data frame carrying an \code{\link{psData}} attribute.
#' @param variable the variable to lag/lead.
#' @param k the number of lags/leads.
#' @seealso \code{\link{psData}}
#' @references Christopher Gandrud, "Slide: one function for lag/lead variables 
#' in data frames, including time-series cross-sectional data", 
#' \url{http://christophergandrud.blogspot.com/2013/05/slide-one-function-for-laglead.html}
#' @examples
#' # Load Reinhart and Rogoff demo data.
#' data(debt)
#' # Convert to "country name-year" panel format.
#' debt = as.panel(debt, "Country", "Year")
#' # Lag real GDP growth by one time period.
#' debt$L1.growth = panel_lag(debt, "growth", -1)
#' # Lead real GDP growth by two time periods.
#' debt$F2.growth = panel_lead(debt, "growth", 2)
#' # Show results.
#' head(debt[, c("Year", "growth", "L1.growth", "F2.growth")])
#' @keywords panel ts
panel_shift = function(data, variable, k = 1) {
#   stopifnot(class(data) == "psData")
#   stopifnot(variable %in% names(data))
  v = tapply(data[, variable], data[, data@design$panel], shift, k)
  v = unlist(v)
  return(v)
}

#' @export
panel_lag = function(data, variable, k = 1) {
  panel_shift(data, variable, - abs(k))
}

#' @export
panel_lead = function(data, variable, k = 1) {
  panel_shift(data, variable, abs(k))
}

#' Linear decay
#'
#' Linear decay function adapted from the \code{\link[doBy]{doBy}} package by 
#' Zachary M. Jones, and modified to handle \code{\link{psData-class}} objects.
#' 
#' @export
#' @param data a data frame of \code{\link{psData-class}}.
#' @param x the variable for which to compute linear decay
#' @param cutpoint the decay cut-point.
#' @author Zachary M. Jones
#' @source Zachary M. Jones, 
#' "Some Time-Series Functions for Panels with Missingness", 
#' \url{http://www.zmjones.com/panel-ts.html}
#' @seealso \code{\link[doBy]{doBy}}
#' @keywords panel ts
panel_decay = function(data, x, cutpoint) {
  data = tapply(data[, x], data[, data@design$panel], panel_decay, d = cutpoint)
  return(data)
}

#' Time since event
#'
#' Time since event function adapted from the \code{\link[doBy]{doBy}} package by 
#' Zachary M. Jones, and modified to understand the \code{\link{psData}} 
#' attribute.
#' 
#' @export
#' @param data a data frame with the \code{\link{psData}} attribute.
#' @param x the variable for which to compute time since event.
#' @author Zachary M. Jones
#' @source Zachary M. Jones, 
#' "Some Time-Series Functions for Panels with Missingness", 
#' \url{http://www.zmjones.com/panel-ts.html}
#' @seealso \code{\link[doBy]{doBy}}
#' @keywords panel ts
panel_tse = function(data, x) {
  data = tapply(data[, x], data[, data@design$panel], panel_tse)
  return(data)
}

#' Subset a \code{\link{psData}} object
#'
#' No specific behaviour for the moment, just checking that we are hacking the
#' \code{subset} method properly by redirecting \code{psData} objects to here.
#' 
#' The evaluation method of the \code{eval} function call is explained
#' at \url{https://github.com/hadley/devtools/wiki/Computing-on-the-language#non-standard-evaluation-in-subset}.
#' 
#' @export
#' @param data a data frame of \code{\link{psData-class}}.
#' @param formula a logical formula to subset to.
#' @param select the names of the variables to keep.
#' @param drop passed on to \code{[} indexing operator.
#' @param ... other methods passed to \code{\link{subset}}
#' @return a data frame
#' @seealso \code{\link{psData}}, \code{\link{subset}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Load Reinhart and Rogoff demo data.
#' data(debt)
#' # Convert to "country name-year" panel format.
#' debt = as.panel(debt, "Country", "Year")
#' # Subset to negative growth country-years.
#' panel_subset(debt, growth < 0)
#' # Plot debt-to-growth by decade.
#' if(all(require(ggplot2), require(MASS), require(splines))) {
#'   library(splines)
#'   library(MASS)
#'   debt$Decade = 10 * debt$Year %/% 10
#'   qplot(data = debt, x = growth, y = ratio, 
#'         group = Country, color = ratio < 90,
#'         alpha = I(.5)) + 
#'     facet_wrap(~ Decade, scales = "free_x", nrow = 2) + 
#'     geom_line(alpha = .25) +
#'     geom_smooth(aes(group = NULL, color = NULL),
#'                 method ="rlm", formula = y ~ ns(x, 3),
#'                 alpha = .25, fill = "lightblue") +
#'     scale_color_brewer("", palette = "Set1") +
#'     guides(color = FALSE) +
#'     labs(y = "debt/GDP ratio (ratios above 90% in red)", x = "real GDP growth") +
#'     theme_bw(14)
#' }
#' @name panel_subset
#' @keywords psData
panel_subset = function(data, formula, select = names(data), drop = FALSE) {

  keep = eval(substitute(formula), data)
  data = data[keep, select, drop = drop]
  return(data)
}
