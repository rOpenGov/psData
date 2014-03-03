#' Downloads the Database of Political Institutions (DPI)
#' 
#' Downloads the Database of Political Institutions (DPI) data set. It keeps specified variables and creates a standard country ID variable that can be used for merging the data with other data sets.
#' 
#' @param url, read, var.n, var.t, ... \code{\link{get}} parameters
#' @details Note: a bit.ly URL is used to shorten the Stata formatted data set's URL due to CRAN requirements.
#'
#' @return a data frame
#'    
#' @examples
#' \dontrun{
#' # Download full data set
#' DpiData <- get_dpi()
#' 
#' # Create data frame with only the military variable
#' DpiSub <- get_dpi(vars = 'military', OutCountryID = 'imf')
#' }
#'  
#' @seealso \code{\link{countrycode}}, \code{\link{CountryID}}, \code{\link{WinsetCreator}}
#'
#' @importFrom foreign read.dta
#' @keywords dpi data csts
#'    
#' @export
get_dpi = function(url = "http://bit.ly/1jZ3nmM", 
                   var.n = "countryname", var.t = "year", read = "dta", ...) {
  return(get_data(url, var.n, var.t, read, ...))
}
