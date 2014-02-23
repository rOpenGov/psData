#' Downloads Polity IV
#' 
#' Downloads the Polity IV data set. It keeps specified variables and creates a standard country ID variable that can be used for merging the data with other data sets.
#' 
#' @param url character string. The URL for the Polity IV data set you would like to download. Note: it must be for the SPSS version of the file.
#' @param vars character vector containing the variables to keep. If \code{vars = NULL} then the entire data set is returned. Note that the \code{country} and \code{year} variables are always returned.  
#' @param OutCountryID character string. The type of country ID you would like to include in the output file along with the country name. See \code{\link{countrycode}} for available options. 
#' @param standardCountryName logical. Whether or not to standardise the country names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' @param na.rm logical. Drop observations where \code{OutCountryID} is \code{NA}.
#' @param duplicates character specifying how to handle duplicated country-year observations. Can be set to \code{none} to do nothing, \code{message} to simply report duplicates, \code{drop} to report and drop duplicates, and \code{return} to return a data frame with only duplicated observations (see also \code{fromLast}).
#' @param fromLast logical indicating if duplication should be considered from the reverse side. Only relevant if \code{duplicates = 'drop'} or \code{duplicates = 'out'}.
#'   
#'    
#' @examples
#' \dontrun{
#' # Download full data set
#' PolityData <- PolityGet()
#' 
#' # Create data frame with only the main Polity democracy variable (polity2)
#' Polity2Data <- PolityGet(vars = 'polity2', 
#'                          OutCountryID = 'imf')
#' }
#'  
#' @seealso \code{\link{countrycode}}, \code{\link{CountryID}}, \code{\link{WinsetCreator}}
#'
#' @importFrom foreign read.spss
#'    
#' @export

PolityGet <- function(url = 'http://www.systemicpeace.org/inscr/p4v2012.sav', 
                      vars = NULL, OutCountryID = 'iso2c', standardCountryName = TRUE, 
                      na.rm = TRUE, duplicates = 'message', fromLast = FALSE){
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
    PolityData <- CountryID(data = PolityData, OutCountryID = OutCountryID,
                    timeVar = 'year', duplicates = duplicates, 
                    standardCountryName = standardCountryName, fromLast = fromLast)
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
#' @param standardCountryName logical. Whether or not to standardise the country names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' @param na.rm logical. Drop observations where \code{OutCountryID} is \code{NA}.
#' @param duplicates character specifying how to handle duplicated country-year observations. Can be set to \code{none} to do nothing, \code{message} to simply report duplicates, \code{drop} to report and drop duplicates, and \code{return} to return a data frame with only duplicated observations (see also \code{fromLast}).
#' @param fromLast logical indicating if duplication should be considered from the reverse side. Only relevant if \code{duplicates = 'drop'} or \code{duplicates = 'out'}.
#'
#' @details Note: a bit.ly URL is used to shorten the Stata formatted data set's URL due to CRAN requirements.
#'   
#'    
#' @examples
#' \dontrun{
#' # Download full data set
#' DpiData <- DpiGet()
#' 
#' # Create data frame with only the military variable
#' DpiSub <- DpiGet(vars = 'military', 
#'                  OutCountryID = 'imf')
#' }
#'  
#' @seealso \code{\link{countrycode}}, \code{\link{CountryID}}, \code{\link{WinsetCreator}}
#'
#' @importFrom foreign read.dta
#'    
#' @export

DpiGet <- function(url = 'http://bit.ly/1jZ3nmM', vars = NULL, OutCountryID = 'iso2c', 
                   standardCountryName = TRUE, na.rm = TRUE, 
                   duplicates = 'message', fromLast = FALSE){
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
        
    # Include new country ID variable
    DpiData <- CountryID(data = DpiData, 
                    OutCountryID = OutCountryID, countryVar = 'countryname',
                    timeVar = 'year', duplicates = duplicates, 
                    standardCountryName = standardCountryName,fromLast = fromLast)

    # Drop NAs for OutCountryID
    if (isTRUE(na.rm)){
        DpiData <- DropNA.psData(data = DpiData, 
                        Var = OutCountryID)
    }
    return(DpiData)
}

#' Download and combine Reinhart and Rogoff's (2010) crisis dummy variables into one data frame
#' 
#' @param urls URLs for each Excel file in the Reinhart and Rogoff data set. See \url{http://www.carmenreinhart.com/data/browse-by-topic/topics/7/}.
#' @param OutCountryID character string. The type of country ID you would like to include in the output file along with the country name. See \code{\link{countrycode}} for available options. 
#' @param standardCountryName logical. Whether or not to standardise the country names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' 
#' @return Returns a data frame with the following columns:
#' \itemize{
#'   \item{\code{iso2c}: }{The ISO two letter country code identifying the country. This can be changed to another country ID system using \code{OutCountryID}}
#'   \item{\code{country}: }{Country names.}
#'   \item{\code{year}: }{The year.}
#'   \item{\code{RR_Independence}: }{Year of independence.}
#'   \item{\code{RR_CurrencyCrisis}: }{Currency crisis.}
#'   \item{\code{RR_InflationCrisis}: }{Inflation crisis.}
#'   \item{\code{RR_StockMarketCrash}: }{Stock market crash.}
#'   \item{\code{RR_SovDebtCrisisDom}: }{Domestic sovereign debt crisis.}
#'   \item{\code{RR_SovDebtCrisisExt}: }{External sovereign debt crisis.}
#'   \item{\code{RR_BankingCrisis}: }{Banking crisis.}
#'   \item{\code{RR_YearlyCrisisTally}: }{Total number of crises per year.}
#'  }
#'  
#'  @examples
#'  \dontrun{
#'  # RRDummies <- RRCrisisGest()
#'  }
#'
#'  @source
#'  Reinhart, Camen M. and Kenneth S. Rogoff, ''From Financial Crash to Debt Crisis,'' NBER Working Paper 15795, March 2010. Forthcoming in American Economic Review.  
#' 
#' @importFrom xlsx loadWorkbook
#' @importFrom xlsx getSheets
#' @importFrom xlsx read.xlsx
#' @importFrom DataCombine MoveFront
#' @importFrom DataCombine DropNA
#' @export

RRCrisisGet <- function(urls = c(
  'http://www.carmenreinhart.com/user_uploads/data/22_data.xls',
  'http://www.carmenreinhart.com/user_uploads/data/35_data.xls', 
  'http://www.carmenreinhart.com/user_uploads/data/23_data.xls',
  'http://www.carmenreinhart.com/user_uploads/data/25_data.xls'), 
  OutCountryID = 'iso2c', 
  standardCountryName = TRUE){
  
  OutData <- data.frame()
  
  for (i in urls){
    tmp <- tempfile()
    download.file(i, tmp)
    WB <- getSheets(loadWorkbook(tmp))
    
    # Load workbook and find relevant sheet names
    WBNames <- names(WB)
    Droppers <- c('Contents', 'CrisisDefinition', 'CrisisDefinitions', 'Sheet1', 'Sheet3')
    WBNames <- WBNames[!is.element(WBNames, Droppers)]
    
    for (u in WBNames){
      Temp <- read.xlsx(tmp, u)
      # Keep only the year and crisis indicators
      Temp <- Temp[13:nrow(Temp), c(1:9)]
      
      # Extract the country name
      if (u == 'UK'){
        Temp$country <- 'United Kingdom'
      }
      else if (u == 'US'){
        Temp$country <- 'United States'
      }
      else if (u != 'US' & u != 'UK'){
        TempNames <- names(Temp[1])
        CountryName <- gsub('([A-z]+).*', '\\1', TempNames)
        Temp$country <- CountryName
      }
      Temp <- MoveFront(Temp, 'country')
      
      # Rename variables
      names(Temp) <- c('country', 'year', 'RR_Independence', 'RR_CurrencyCrisis', 'RR_InflationCrisis',
                       'RR_StockMarketCrash', 'RR_SovDebtCrisisDom', 'RR_SovDebtCrisisExt',
                       'RR_BankingCrisis', 'RR_YearlyCrisisTally')
      
      message(paste0('Cleaning up Excel sheet for ', u, '.\n'))
      Temp <- DropNA(Temp, c('year', 'RR_BankingCrisis'), message = FALSE)
      OutData <- rbind(OutData, Temp)
    }
  }
  # Clean up country names and add ID
  OutData$country[OutData$country == 'New'] <- 'New Zealand'
  OutData$country[OutData$country == 'South'] <- 'South Africa'
  OutData$country[OutData$country == 'Sri'] <- 'Sri Lanka'
  OutData$country[OutData$country == 'Central'] <- 'Central African Republic'
  OutData$country[OutData$country == 'Costa'] <- 'Costa Rica'
  OutData$country[OutData$country == 'Cote'] <- 'Cote dIvoire'
  OutData$country[OutData$country == 'Costa'] <-
    OutData$country[OutData$country == 'Dominican'] <- 'Domincan Republic'
  OutData$country[OutData$country == 'El'] <- 'El Salvador'
  
  OutData <- CountryID(data = OutData, OutCountryID = OutCountryID,
                       timeVar = 'year', standardCountryName = standardCountryName)
  
  # Clean year
  OutData$year <- as.character(OutData$year)
  OutData$year <- as.numeric(OutData$year)

  OutData <- OutData[order(OutData[, OutCountryID], OutData[, 'year']), ]
  
  return(OutData)
}