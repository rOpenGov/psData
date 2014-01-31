#' Downloads Polity IV
#' 
#' Downloads the Polity IV data set. It keeps specified variables and creates a standard country ID variable that can be used for merging the data with other data sets.
#' 
#' @param url character string. The URL for the Polity IV data set you would like to download. Note: it must be for the SPSS version of the file.
#' @param vars character vector containing the variables to keep. If \code{vars = NULL} then the entire data set is returned. Note that the \code{country} and \code{year} variables are always returned.  
#' @param OutCountryID character string. The type of country ID you would like to include in the output file along with the country name. See \code{\link{countrycode}} for available options. 
#' @param na.rm logical. Drop observations where \code{OutCountryID} is \code{NA}.
#'   
#'    
#' @examples
#' # Download full data set
#' PolityData <- PolityGet()
#' 
#' # Create data frame with only the main Polity democracy variable (polity2)
#' Polity2Data <- PolityGet(vars = 'polity2', 
#'                          OutCountryID = 'imf')
#'  
#' @seealso \code{\link{countrycode}}
#'
#' @importFrom foreign read.spss
#'    
#' @export

PolityGet <- function(url = 'http://www.systemicpeace.org/inscr/p4v2012.sav', vars = NULL, OutCountryID = 'iso2c', na.rm = TRUE){
    # Download underlying Polity IV data 
    tmpfile <- tempfile()
    download.file(url, tmpfile)
    PolityData <- read.spss(tmpfile, to.data.frame = TRUE)  
    unlink(tmpfile)
    
    # Ensure that vars are in the data frame

    # Clean up
    # Ensure that vars are in the data frame
    if (!is.null(vars)){
      if (!all(vars %in% names(PolityData))){
        stop('Specified variables not found in data.')
      }
    	Vars <- c('country', 'year', vars)
    	PolityData <- PolityData[, Vars] 
    } 
        
    # Include new country ID variable and standardise country names
    PolityData <- CountryID(data = PolityData, 
                    OutCountryID = OutCountryID)
  # Drop NAs for OutCountryID
    if (isTRUE(na.rm)){
        PolityData <- DropNA.psData(data = PolityData, 
                        Var = OutCountryID)
    }
    return(PolityData)
}


#' Downloads the Database of Political Institutions (DPI)
#' 
#' Downloads the Database of Political Institutions (DPI) data set. It keeps specified variables and creates a standard country ID variable that can be used for merging the data with other data sets.
#' 
#' @param url character string. The URL for the Polity IV data set you would like to download. Note: the link must be to a Stata formated file.
#' @param vars character vector containing the variables to keep. If \code{vars = NULL} then the entire data set is returned. Note that \code{country} and \code{year} variables are always returned.  
#' @param OutCountryID character string. The type of country ID you would like to include in the output file along with the country name. See \code{\link{countrycode}} for available options. 
#' @param na.rm logical. Drop observations where \code{OutCountryID} is \code{NA}.
#'   
#'    
#' @examples
#' # Download full data set
#' DpiData <- DpiGet()
#' 
#' # Create data frame with only the military variable
#' DpiSub <- DpiGet(vars = 'military', 
#'                  OutCountryID = 'imf')
#'  
#' @seealso \code{\link{countrycode}}
#'
#' @importFrom foreign read.dta
#'    
#' @export

DpiGet <- function(url = 'http://siteresources.worldbank.org/INTRES/Resources/469232-1107449512766/DPI2012.dta', vars = NULL, OutCountryID = 'iso2c', na.rm = TRUE){
    # Download underlying Dpi IV data 
    tmpfile <- tempfile()
    download.file(url, tmpfile)
    DpiData <- read.dta(tmpfile)  
    unlink(tmpfile)

    # Clean up
    if (!is.null(vars)){
        if (!all(vars %in% names(DpiData))){
          stop('Specified variables not found in data.')
        }
        Vars <- c('countryname', 'year', vars)
        DpiData <- DpiData[, Vars] 
    } 
        
    # Include new country ID variable and standardise country names
    DpiData <- CountryID(data = DpiData, 
                    OutCountryID = OutCountryID,
                    countryVar = 'countryname')

    # Drop NAs for OutCountryID
    if (isTRUE(na.rm)){
        DpiData <- DropNA.psData(data = DpiData, 
                        Var = OutCountryID)
    }
    return(DpiData)
}