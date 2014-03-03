
#' Get data in all formats read by the default packages
#' 
#' The get_data function requires a URL, a read method ('csv' by default),
#' a panel variable and a time variable. These parameters are easy to pass
#' through little functions like the other get methods in this file.
#' 
#' @param url character string. The URL for the data set you would like to download.
#' @param var.n the main panel variable
#' @param var.t the main time variable
#' @param read the format of the data (\code{csv}, \code{dta}, \code{spss} or any other \code{read.method} function).
#' @param vars character vector containing the variables to keep. If \code{vars = NULL} then the entire data set is returned. Note that the \code{country} and \code{year} variables are always returned.  
#' @param OutCountryID character string. The type of country ID you would like to include in the output file along with the country name. See \code{\link{countrycode}} for available options. 
#' @param standardCountryName logical. Whether or not to standardise the country names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' @param na.rm logical. Drop observations where \code{OutCountryID} is \code{NA}.
#' @param duplicates character specifying how to handle duplicated country-year observations. Can be set to \code{none} to do nothing, \code{message} to simply report duplicates, \code{drop} to report and drop duplicates, and \code{return} to return a data frame with only duplicated observations (see also \code{fromLast}).
#' @param fromLast logical indicating if duplication should be considered from the reverse side. Only relevant if \code{duplicates = 'drop'} or \code{duplicates = 'out'}.
#' @return a data frame
#' @importFrom downloader download
#' @export
get_data = function(url = NULL, var.n = "country", var.t = "year", read = "csv", vars = NULL, OutCountryID = "iso2c", standardCountryName = TRUE, na.rm = TRUE, duplicates = 'message', fromLast = FALSE) {
  
  stopifnot(!is.null(url))
  try_require("downloader")
  try_require("foreign")
  
  tmpfile <- tempfile()
  download(url, tmpfile, mode = "wb")
  
  args = NULL
  if(method == "spss") args = c("to.data.frame" = TRUE)
  data = do.call(paste0("read.", method), list(file = tmpfile, args))
  
  unlink(tmpfile)
  
  # Ensure that vars are in the data frame
  if (!is.null(vars)){
    if (!all(vars %in% names(data))){
      stop('Specified variables not found in data.')
    }
    vars <- c(var.n, var.t, vars)
    data <- data[, vars] 
  } 
  
  # Include new country ID variable
  data <- CountryID(data, 
                    OutCountryID = OutCountryID, countryVar = var.n,
                    timeVar = var.t, duplicates = duplicates, 
                    standardCountryName = standardCountryName,fromLast = fromLast)
  
  # Drop NAs for OutCountryID
  if (isTRUE(na.rm)){
    data <- DropNA.psData(data, Var = OutCountryID)
  }
  return(data)
  
}

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
get_polity4 = function(url = "http://www.systemicpeace.org/inscr/p4v2012.sav", 
                       var.n = "country", var.t = "year", read = "spss", ...) {
  return(get_data(url, var.n, var.t, read, ...))
}

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
#'    
#' @export
get_dpi = function(url = "http://bit.ly/1jZ3nmM", 
                   var.n = "countryname", var.t = "year", read = "dta", ...) {
  return(get_data(url, var.n, var.t, read, ...))
}

#' Data getters
#' 
#' \itemize{
#'   \item Polity IV
#'   \item DPI
#'   }
#'   
#' Override the default parameters only if you know exactly what alternative version 
#' and format of the data you need.
#' 
#' @param url the URL of the data (modify if you need another version of the dataset)
#' @param read the format of the data (e.g. \code{csv}, \code{dta}, \code{spss})
#' @param var.n the panel variable
#' @param var.t the time variable
#' @param ... additional arguments passed to \code{\link{get_data}}
#' @seealso \code{\link{get_data}}, \code{\link{get_polity4}}, \code{\link{get_dpi}}
#' @export
get = function(x, ...) {
  if(x == "polity4") get_polity4(...)
  if(x == "dpi") get_dpi(...)
}
  