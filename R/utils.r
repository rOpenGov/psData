# Lag or lead a variable
# 
# @param x the variable.
# @param shift_by the negative (lag) or positive (lead) size of the shift
# @author TszKin Julian Chan
# @source TszKin Julian Chan, "Generating lag/lead variables", 
# \url{http://ctszkin.com/2012/03/11/generating-a-laglead-variables/}
# @keywords internal ts
shift = function(x, shift_by) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(shift_by))
  # won't work from inside an xtshift call
  #   if(length(shift_by) > 1)
  #     return(sapply(shift_by, shift, x = x))  
  out = NULL
  abs_shift_by = abs(shift_by)
  if(shift_by > 0 )
    out = c(tail(x, -abs_shift_by), rep(NA, abs_shift_by))
  else if (shift_by < 0)
    out = c(rep(NA, abs_shift_by), head(x, -abs_shift_by))
  else
    out = x
  # bugfix
  out = out[1:length(x)]
  return(out)
}

# Linear decay
#
# @param yvar the variable for which to compute time since event.
# @param d the decay cut-point.
# @author Zachary M. Jones
# @source Zachary M. Jones, 
# "Some Time-Series Functions for Panels with Missingness", 
# \url{http://www.zmjones.com/panel-ts.html}
# @keywords internal
decay <- function(yvar, d) {
  yvar[is.na(yvar)] <- 0
  run <- cumsum(yvar)
  tvar = seq_along(yvar)
  run = 0; sum = 0
  for(i in 1:length(tvar)) {
    if(yvar[i] == 1)
      run = run + 1
    
    if(run != 0) {
      event.idx <- which(yvar == 1)
      for(j in 1:length(event.idx)) {
        if(i == (d + event.idx[j])) {
          run = run - 1
        }}}
    sum[i] = run
  }
  return(sum)
}

# Time since event
#
# @param yvar the variable for which to compute time since event.
# @param tvar the time sequence.
# @author Zachary M. Jones
# @source Zachary M. Jones, 
# "Some Time-Series Functions for Panels with Missingness", 
# \url{http://www.zmjones.com/panel-ts.html}
# @keywords internal
panel.tse <- function(yvar, tvar = seq_along(yvar)) {
  if (!(is.numeric(yvar) | is.logical(yvar)))
    stop("yvar must be either numeric or logical")
  
  yvar[is.na(yvar)] <- 0
  event.idx <- which(yvar == 1)
  run <- cumsum(yvar)
  un <- unique(run)
  tlist <- list()
  for (i in 1:length(un)) {
    v <- un[[i]]
    y <- yvar[run == v]
    t <- tvar[run == v]
    t <- t - t[1]
    tlist[[i]] <- t
  }
  
  timeAfterEvent <- unlist(tlist)
  timeAfterEvent[run == 0] <- NA
  run[run == 0] <- NA
  
  return(timeAfterEvent)
}

# Quantize a variable
#
# Cut a variable to its quantiles, with error correction for the quantiles argument if it is 
# superior to the number of unique values in the data.
#
# Inspired by several other similar helper functions in other packages, and by 
# the \code{xtile} function in Stata. Used mostly in \code{panel_map} (coming).
# 
# @param x variable
# @param q quantiles
# @param levels whether to relabel the levels to \code{"xmin-xmax"}, where 
# \code{xmin} and \code{xmax} are the numeric bounds of the level. This will 
# remove some display issues with scientific notation in level names.
# @seealso \{code\link[ggplot2]{cut_number}}, 
# \{code\link[ggplot2]{cut_interval}}, \{code\link[questionr]{quant.cut}}
# @keywords internal
quantize <- function(x, q, levels = FALSE) {
  stopifnot(q > 0 & length(x) > 0)
  if(q >= length(unique(x))) {
    q = length(unique(x)) - 1
    warning("only ", q + 1, " values exist in the data")
  }
  y = cut(x,
          quantile(x, 
                   probs = seq(0, 1, by = 1/q), 
                   na.rm = TRUE),
          include.lowest = TRUE,
          ordered_result = TRUE)
  if(levels)
    levels(y) = paste(tapply(x, y, min), tapply(x, y, max), sep = "-")
  return(y)
}

# Standardize a variable
#
# Standardize a variable to (0,1).
# 
# @param x variable
# @keywords internal
std01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Quietly try to require a package
#
# Quietly require a package, returning an error message if that package is not installed.
# Code snippet taken from \code{\link[ggplot2]{ggplot2}} 0.9.3.1.
# 
# @param package name of package
# @author Hadley Wickham
# @references Wickham, H. 2009. 
# \emph{ggplot2: Elegant graphics for data analysis}, New York, Springer.
# @source \url{https://github.com/hadley/ggplot2/blob/master/R/utilities.r#L46}
# @keywords internal
try_require <- function(package) {
  available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts=FALSE)))
  missing <- package[!available]

  if (length(missing) > 0)
    stop(paste(missing, collapse=", "), " package required for this functionality.  Please install and try again.", call. = FALSE)
}

# Sort data frame
#
# Convenience method for sorting a data frame using the given variables..
# Code snippet taken from \code{\link[reshape]{reshape}} 0.8.4.
# 
# @param data data frame to sort
# @param variables to use for sorting
# @author Hadley Wickham
# @references Wickham, H. 2007. "Reshaping data with the reshape package." 
# \emph{Journal of Statistical Software} 21(12), 2007.
# \url{https://github.com/hadley/reshape/}
# @source \url{https://github.com/hadley/reshape/blob/reshape0.8/R/utils.r#L82}
# @keywords internal
sort_df <- function (data, vars = names(data)) 
{
  if (length(vars) == 0 || is.null(vars)) 
    return(data)
  data[do.call("order", data[, vars, drop = FALSE]), , drop = FALSE]
}

#' Drop rows from a data frame with missing values in the OutCountryID variable.
#'
#' @param data a data frame object.
#' @param Var a character vector naming the variables you would like to have only non-missing (NA) values.
#' 
#' @source Largely based on \code{DropNA} from the \code{DataCombine} package.
#'
#' @keywords internal
#' @export

DropNA.psData <- function(data, Var)
{
  # Find term number
  DataNames <- names(data)
  TestExist <- Var %in% DataNames
  if (!all(TestExist)){
          stop("Variable(s) not found in the data frame.")
  }

  # Drop if NA
  if (length(Var) == 1){
          DataNoNA <- data[!is.na(data[, Var]), ]

          DataVar <- data[, Var]
          DataNA <- DataVar[is.na(DataVar)]
          TotalDropped <- length(DataNA)
  }
  else{
          RowNA <- apply(data[, Var], 1, function(x){any(is.na(x))})
          DataNoNA <- data[!RowNA, ]

          TotalDropped <- sum(RowNA)
  }        

  message(paste0(TotalDropped, " observations dropped based on missing values of the standardised ID variable.\n\n"))
  return(DataNoNA)
}

#' Autodetect a date format
#' 
#' Looks for perfect matches between the values of a variable and its converted 
#' values to a date format handled by the \code{\link{lubridate}} package.
#' @return the fraction of perfect matches
#' @keywords internal
#' @export
try_date = function(x, date = "%Y") {

  try_require("lubridate")

  # crosstabulate old/new values
  t = table(x, parse_date_time(x, date, quiet = TRUE))
  
  # fraction of perfect matches
  t = sum(diag(t)) / sum(t)
  
  if(is.nan(t))
    return(0)
  else
    return(t)

}

#' Autodetect a country code
#' 
#' Looks for perfect matches between the values of a variable and its converted 
#' values from a list of country codes handled by the \code{\link{countrycode}} 
#' package. The codes are assessed against their ISO-3C translation.
#' @return the fraction of perfect matches
#' @keywords internal
#' @export
try_countrycode = function(x, format = "iso3n") {
  
  try_require("countrycode")
  
  # crosstabulate old/new values
  t = table(x, countrycode(x, origin = format, destination = "iso3n", warn = FALSE))
  
  # fraction of perfect matches
  t = sum(diag(t)) / sum(t)
  
  if(is.nan(t))
    return(0)
  else
    return(t)
  
}

# the two functions could be merged if the try_require calls could be removed
