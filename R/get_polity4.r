#' Downloads Polity IV
#' 
#' Downloads the Polity IV data set. It keeps specified variables and creates a standard country ID variable that can be used for merging the data with other data sets.
#' 
#' @param url, read, var.n, var.t, ... \code{\link{get}} parameters
#' @return a data frame   
#'    
#' @examples
#' \dontrun{
#' # Download full data set
#' PolityData <- get_polity4()
#' 
#' # Create data frame with only the main Polity democracy variable (polity2)
#' Polity2Data <- get_polity(vars = 'polity2', OutCountryID = 'imf')
#' }
#'  
#' @seealso \code{\link{countrycode}}, \code{\link{CountryID}}, \code{\link{WinsetCreator}}
#'
#' @importFrom foreign read.spss
#'    
#' @export
#' @keywords polity data csts
get_polity4 = function(url = "http://www.systemicpeace.org/inscr/p4v2012.sav", 
                       var.n = "country", var.t = "year", read = "spss", ...) {
  return(get_data(url, var.n, var.t, read, ...))
}
