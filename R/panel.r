
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
#' # Load QOG demo datasets.
#' data(qog.demo)
#' # Subset to a short series.
#' QOG = panel_subset(qog.ts.demo, year %in% 2008:2012 & cname == "United States")
#' # Lag by one time period.
#' QOG$L1.wdi_gdpc = panel_lag(QOG, "wdi_gdpc", -1)
#' # Lead by two time periods.
#' QOG$F2.wdi_gdpc = panel_lead(QOG, "wdi_gdpc", 2)
#' # Check results.
#' QOG[, c("year", "wdi_gdpc", "L1.wdi_gdpc", "F2.wdi_gdpc")]
#' # Full method.
#' cbind(QOG, sapply(-2:2, panel_shift, data = QOG, variable = "wdi_hec"))
#' @keywords panel ts
panel_shift <- function(data, variable, k = 1) {
  stopifnot(class(data) == "psData")
  stopifnot(variable %in% names(data))
  ccode = data@design$panel
  
  v = tapply(data[, variable], data[, ccode], shift, k)
  v = unlist(v)
  return(v)
}

#' @export
panel_lag <- function(data, variable, k = 1) {
  panel_shift(data, variable, - abs(k))
}

#' @export
panel_lead <- function(data, variable, k = 1) {
  panel_shift(data, variable, abs(k))
}

#' Linear decay
#'
#' Linear decay function adapted from the \code{\link[doBy]{doBy}} package by 
#' Zachary M. Jones, and modified to understand the \code{\link{psData}} 
#' attribute.
#' 
#' @export
#' @param data a data frame with the \code{\link{psData}} attribute.
#' @param x the variable for which to compute linear decay
#' @param cutpoint the decay cut-point.
#' @author Zachary M. Jones
#' @source Zachary M. Jones, 
#' "Some Time-Series Functions for Panels with Missingness", 
#' \url{http://www.zmjones.com/panel-ts.html}
#' @seealso \code{\link[doBy]{doBy}}
#' @keywords panel ts
panel_decay <- function(data, x, cutpoint) {
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
panel_tse <- function(data, x) {
  data = tapply(data[, x], data[, data@design$panel], panel_tse)
  return(data)
}
