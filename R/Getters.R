#' Downloads Polity IV
#'
#' Downloads the Polity IV data set. It keeps specified variables and creates a
#' standard country ID variable that can be used for merging the data with other
#' data sets.
#'
#' @param url character string. The URL for the Polity IV data set you would
#' like to download. Note: it must be for the SPSS version of the file.
#' @param vars character vector containing the variables to keep. If
#' \code{vars = NULL} then the entire data set is returned. Note that the
#' \code{country} and \code{year} variables are always returned.
#' @param OutCountryID character string. The type of country ID you would like
#' to include in the output file along with the country name. See
#' \code{\link{countrycode}} for available options.
#' @param standardCountryName logical. Whether or not to standardise the country
#' names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' @param na.rm logical. Drop observations where \code{OutCountryID} is
#' \code{NA}.
#' @param duplicates character specifying how to handle duplicated country-year
#' observations. Can be set to \code{none} to do nothing, \code{message} to
#' simply report duplicates, \code{drop} to report and drop duplicates, and
#' \code{return} to return a data frame with only duplicated observations
#' (see also \code{fromLast}).
#' @param fromLast logical indicating if duplication should be considered
#' from the reverse side. Only relevant if \code{duplicates = 'drop'} or
#' \code{duplicates = 'out'}.
#'
#' @return a data frame
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
#' @seealso \code{\link{countrycode}}, \code{\link{CountryID}},
#' \code{\link{WinsetCreator}}
#'
#' @importFrom rio import
#'
#' @export

PolityGet <- function(url = 'http://www.systemicpeace.org/inscr/p4v2012.sav',
                      vars = NULL, OutCountryID = 'iso2c',
                      standardCountryName = TRUE,
                      na.rm = TRUE, duplicates = 'message', fromLast = FALSE){
    # Download underlying Polity IV data
    PolityData <- import(url)

    # Clean up
    # Ensure that vars are in the data frame
    if (!is.null(vars)) {
      if (!all(vars %in% names(PolityData))) {
        stop('Specified variables not found in data.')
      }
        Vars <- c('country', 'year', vars)
        PolityData <- PolityData[, Vars]
    }

    # Include new country ID variable and standardise country names
    PolityData <- CountryID(data = PolityData, OutCountryID = OutCountryID,
                    timeVar = 'year', duplicates = duplicates,
                    standardCountryName = standardCountryName,
                    fromLast = fromLast)
  # Drop NAs for OutCountryID
    if (isTRUE(na.rm)) {
        PolityData <- DropNA.psData(data = PolityData,
                                    timeVar='year',
                                    OutCountryID=OutCountryID)
    }
    return(PolityData)
}


#' Downloads the Database of Political Institutions (DPI)
#'
#' Downloads the Database of Political Institutions (DPI) data set. It keeps
#' specified variables and creates a standard country ID variable that can be
#' used for merging the data with other data sets.
#'
#' @param url character string. The URL for the Polity IV data set you would
#' like to download. Note this is exclusively to download previous, IMF hosted,
#' versions of the data set. If a value is not supplied, then the 2015 IDB 
#' hosted version will be downloaded. If a link is supplied it must be to a 
#' Stata formated file.
#' @param vars character vector containing the variables to keep. If
#' \code{vars = NULL} then the entire data set is returned. Note that
#' \code{country} and \code{year} variables are always returned.
#' @param OutCountryID character string. The type of country ID you would like
#' to include in the output file along with the country name. See
#' \code{\link{countrycode}} for available options.
#' @param standardCountryName logical. Whether or not to standardise the country
#' names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' @param na.rm logical. Drop observations where \code{OutCountryID} is
#' \code{NA}.
#' @param duplicates character specifying how to handle duplicated country-year
#' observations. Can be set to \code{none} to do nothing, \code{message} to
#' simply report duplicates, \code{drop} to report and drop duplicates, and
#' \code{return} to return a data frame with only duplicated observations
#' (see also \code{fromLast}).
#' @param fromLast logical indicating if duplication should be considered from
#' the reverse side. Only relevant if \code{duplicates = 'drop'} or
#' \code{duplicates = 'out'}.
#'
#'
#' @return a data frame
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
#' @seealso \code{\link{countrycode}}, \code{\link{CountryID}},
#' \code{\link{WinsetCreator}}
#'
#' @importFrom rio import
#' @importFrom utils download.file unzip
#'
#' @export

DpiGet <- function(url, vars = NULL,
                   OutCountryID = 'iso2c',
                   standardCountryName = TRUE, na.rm = TRUE,
                   duplicates = 'message', fromLast = FALSE){
    # Download underlying Dpi IV data
    if (missing(url)){
        message('Downloading the 2015 DPI from: http://www.iadb.org/en/research-and-data/publication-details,3169.html?pub_id=IDB-DB-121\n\n')
        url <- 'http://www.iadb.org/document.cfm?pubDetail=1&id=40094628' 
        
        tmp_file <- tempfile()
        download.file(url, tmp_file)
        
        con <- unzip(tmp_file, files = 'DPI2015/DPI2015_stata11.dta')
        
        DpiData <- import(con)
    }
    
    else if (!missing(url)) {
        DpiData <- import(url, format = 'dta')
        DpiData <- labelDataset(DpiData)
    }

    # Clean up
    if (!is.null(vars)) {
        if (!all(vars %in% names(DpiData))) {
          stop('Specified variables not found in data.')
        }
        Vars <- c('countryname', 'year', vars)
        DpiData <- DpiData[, Vars]
    }

    # Include new country ID variable
    DpiData <- CountryID(data = DpiData,
                    OutCountryID = OutCountryID, countryVar = 'countryname',
                    timeVar = 'year', duplicates = duplicates,
                    standardCountryName = standardCountryName,
                    fromLast = fromLast)

    # Drop NAs for OutCountryID
    if (isTRUE(na.rm)) {
        DpiData <- DropNA.psData(data = DpiData,
                                 countryVar = 'countryname', timeVar='year',
                                 OutCountryID=OutCountryID)
    }
    return(DpiData)
}

#' Download and combine Reinhart and Rogoff's (2010) crisis dummy variables into
#' one data frame
#'
#' @param urls URLs for each Excel file in the Reinhart and Rogoff data set. See
#' \url{http://www.carmenreinhart.com/data/browse-by-topic/topics/7/}.
#' @param OutCountryID character string. The type of country ID you would like
#' to include in the output file along with the country name. See
#' \code{\link{countrycode}} for available options.
#' @param message logical. Whether or not to notify you which of sheets are
#' being cleaned and organised.
#' @param standardCountryName logical. Whether or not to standardise the country
#' names variable based on \code{country.name} from  \code{\link{countrycode}}.
#'
#' @return Returns a data frame with the following columns:
#' \itemize{
#'   \item{\code{iso2c}: }{The ISO two letter country code identifying the
#' country. This can be changed to another country ID system using
#' \code{OutCountryID}}
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
#' @examples
#' \dontrun{
#'  RRDummies <- RRCrisisGest()
#'  }
#'
#' @source
#' Reinhart, Camen M. and Kenneth S. Rogoff, ''From Financial Crash to Debt
#' Crisis,'' NBER Working Paper 15795, March 2010. Forthcoming in American
#' Economic Review.
#'
#' @importFrom xlsx loadWorkbook getSheets read.xlsx
#' @importFrom DataCombine MoveFront DropNA
#' @export

RRCrisisGet <- function(urls = c(
    'http://www.carmenreinhart.com/user_uploads/data/22_data.xls',
    'http://www.carmenreinhart.com/user_uploads/data/35_data.xls',
    'http://www.carmenreinhart.com/user_uploads/data/23_data.xls',
    'http://www.carmenreinhart.com/user_uploads/data/25_data.xls'),
    OutCountryID = 'iso2c', message = TRUE,
    standardCountryName = TRUE){

    OutData <- data.frame()

  for (i in urls){
    tmpfile <- tempfile()
    download.file(i, tmpfile)
    WB <- getSheets(loadWorkbook(tmpfile))

    # Load workbook and find relevant sheet names
    WBNames <- names(WB)
    Droppers <- c('Contents', 'CrisisDefinition', 'CrisisDefinitions', 'Sheet1',
                  'Sheet3')
    WBNames <- WBNames[!is.element(WBNames, Droppers)]

    for (u in WBNames){
      Temp <- read.xlsx(tmpfile, u)
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
      names(Temp) <- c('country', 'year', 'RR_Independence',
                       'RR_CurrencyCrisis', 'RR_InflationCrisis',
                       'RR_StockMarketCrash', 'RR_SovDebtCrisisDom',
                       'RR_SovDebtCrisisExt',
                       'RR_BankingCrisis', 'RR_YearlyCrisisTally')
      if (isTRUE(message)){
        message(paste0('Cleaning up Excel sheet for ', u, '.\n'))
      }
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
                       timeVar = 'year',
                        standardCountryName = standardCountryName)

  # Clean year
  OutData$year <- as.character(OutData$year)
  OutData$year <- as.numeric(OutData$year)

  OutData <- OutData[order(OutData[, OutCountryID], OutData[, 'year']), ]

  return(OutData)
}

#' Downloads Dreher's data set of IMF programs and World Bank projects
#' (1970-2011)
#'
#' Downloads Dreher's data set of IMF programs and World Bank projects
#' (1970-2011). It keeps specified variables and creates a standard country ID
#' variable that can be used for merging the data with other data sets.
#'
#' @param url character string. The URL for the Dreher data set you would like
#' to download. Note: it must be for the xlx version of the file. Currently only
#' the 1970-2011 version is supported.
#' @param sheets character vector of the Excel sheets (variables) that you would
#' like to return. See Details for more information.
#' @param OutCountryID character string. The type of country ID you would like
#' to include in the output file along with the country name. See
#' \code{\link{countrycode}} for available options.
#' @param standardCountryName logical. Whether or not to standardise the country
#' names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' @param message logical. Whether or not to notify you which of sheets are
#' being cleaned and organised.
#'
#' @details Using the \code{sheets} argument you can select which variables to
#' download from their individual workbook seets in the original data set.
#' These include:
#' \itemize{
#'  \item{\code{WB other agreed}: }{Number of World Bank projects agreed, other than technical or adjustment.}
#'  \item{\code{WB technical agreed}: }{Number of World Bank technical projects agreed.}
#'  \item{\code{WB adjustment agreed}: }{Number of World Bank adjustment projects agreed.}
#'  \item{\code{WB environment agreed}: }{Number of World Bank environmental projects agreed.}
#'  \item{\code{WB adjustment 5}: }{Number of World Bank adjustment projects in effect for at least 5 months in a particular year.}
#'  \item{\code{IMF SBA}: }{IMF Standby Arrangement agreed, dummy.}
#'  \item{\code{IMF EFF}: }{IMF Extended Fund Facility Arrangement agreed, dummy.}
#'  \item{\code{IMF SAF}: }{IMF Structural Adjustment Facility Arrangement agreed, dummy.}
#'  \item{\code{IMF PRGF}: }{IMF Poverty Reduction and Growth Facility Arrangement agreed, dummy.}
#'  \item{\code{IMF SBA 5}: }{IMF Standby Arrangement in effect for at least 5 months in a particular year, dummy.}
#'  \item{\code{IMF EFF 5}: }{IMF Extended Fund Facility Arrangement in effect for at least 5 months in a particular year, dummy.}
#'  \item{\code{IMF SAF 5}: }{IMF Structural Adjustment Facility Arrangement in effect for at least 5 months in a particular year, dummy.}
#'  \item{\code{IMF PRGF 5}: }{IMF Poverty Reduction and Growth Facility Arrangement in effect for at least 5 months in a particular year, dummy.}
#' }
#' @return a data frame
#'
#' @examples
#' \dontrun{
#' # Download 'WB other agreed', 'WB environment agreed'
#' # These are the default sheets to gather
#' WBPrograms <- IMF_WBGet()
#' }
#'
#' @source Data website: \url{http://www.uni-heidelberg.de/fakultaeten/wiso/awi/professuren/intwipol/datasets_en.html}.
#'
#' When using the IMF data, please cite:
#'
#' Dreher, Axel, 2006, IMF and Economic Growth: The Effects of Programs, Loans,
#' and Compliance with Conditionality, World Development 34, 5: 769-788.
#'
#' When using the World Bank data, please cite:
#'
#' Boockmann, Bernhard and Axel Dreher, 2003, The Contribution of the IMF and
#' the World Bank to Economic Freedom, European Journal of Political Economy
#' 19, 3: 633-649.
#'
#' @importFrom xlsx loadWorkbook
#' @importFrom xlsx getSheets
#' @importFrom xlsx read.xlsx
#' @importFrom DataCombine VarDrop
#' @importFrom reshape2 melt
#' @export

IMF_WBGet <- function(url = 'http://axel-dreher.de/Dreher%20IMF%20and%20WB.xls',
                   sheets = c('WB other agreed', 'WB environment agreed'),
                   OutCountryID = 'iso2c', message = TRUE,
                   standardCountryName = TRUE){
  # Download full Dreher IMF program data set
  tmpfile <- tempfile()
  download.file(url, tmpfile)

  # Select sheet
  WB <- getSheets(loadWorkbook(tmpfile))
  WBNames <- names(WB)

  # Error if desired sheet is not in the data set.
  TestExist <- sheets %in% WBNames
  if (!all(TestExist)){
    stop("Sheets(s) not found in the data set.")
  }

  FullDF <- data.frame()
  for (i in sheets){
    VarName <- gsub(' ', '.', i)
    if (isTRUE(message)){
      message(paste0('Cleaning: ', VarName, '.\n'))
    }
    if (i != 'WB environment agreed'){
      # Extract sheet
      OneSheet <- read.xlsx(tmpfile, i)
      OneSheet <- melt(OneSheet, id.vars = c('Country.Code', 'Country.Name'))

      # Clean
      OneSheet$variable <- gsub('X', '', as.character(OneSheet$variable))
      OneSheet$variable <- as.numeric(OneSheet$variable)
      if (class(OneSheet$value) == 'character'){
        OneSheet$value <- gsub('\\.', '', OneSheet$value)
        OneSheet$value <- as.numeric(OneSheet$value)
      }

      OneSheet <- CountryID(data = OneSheet, OutCountryID = OutCountryID,
                   countryVar = 'Country.Name', duplicates = 'none',
                   standardCountryName = standardCountryName)
      OneSheet <- VarDrop(OneSheet, 'Country.Code')
      names(OneSheet) <- c(OutCountryID, 'country', 'year', VarName)

    }
    else if (i == 'WB environment agreed'){ # originally in country-year format
      # Extract sheet
      OneSheet <- read.xlsx(tmpfile, i)

      OneSheet <- CountryID(data = OneSheet, OutCountryID = OutCountryID,
                   countryVar = 'country', duplicates = 'none',
                   standardCountryName = standardCountryName)
      OneSheet <- VarDrop(OneSheet, 'code')
      names(OneSheet) <- c(OutCountryID, 'country', 'year', VarName)
    }
    # Merge data frames together
    if (ncol(FullDF) == 0){
      FullDF <- OneSheet
    }
    else if (ncol(FullDF) != 0) {
      FullDF <- merge(FullDF, OneSheet, by = c('country', 'year'))
      if (paste0(OutCountryID, '.x') %in% names(FullDF)){
        FullDF <- FullDF[, !(names(FullDF) %in% paste0(OutCountryID, '.y'))]
        FullDF <- MoveFront(FullDF, paste0(OutCountryID, '.x'))
        FinalNames <- names(FullDF)
        FinalNames <- FinalNames[-1]
        names(FullDF) <- c(OutCountryID, FinalNames)
      }
    }
  }
  # Final Clean
  FullDF <- FullDF[order(FullDF[, OutCountryID], FullDF[, 'year']), ]
  return(FullDF)
}

#' Downloads the Democracy and Dictatorship data set
#'
#' Downloads the Democracy and Dictatorship data set. It keeps specified
#' variables and creates a standard country ID variable that can be used for
#' merging the data with other data sets.
#' See the codebook at the authors' website
#' \url{https://sites.google.com/site/joseantoniocheibub/datasets/democracy-and-dictatorship-revisited}
#' (Direct link to codebook: \url{http://uofi.box.com/shared/static/e6e312753fbc609fc379.pdf})
#'
#' @param url character string. The URL for the Democracy and Dictatorship data
#' set you would like to download. Note: it must be for the Stata version of
#' the file.
#' @param vars character vector containing the variables to keep. If
#' \code{vars = NULL} then the entire data set is returned. Note that the
#' \code{country} and \code{year} variables are always returned.
#' @param OutCountryID character string. The type of country ID you would like
#' to include in the output file along with the country name. See
#' \code{\link{countrycode}} for available options.
#' @param standardCountryName logical. Whether or not to standardise the country
#' names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' @param na.rm logical. Drop observations where \code{OutCountryID} is
#' \code{NA}.
#' @param duplicates character specifying how to handle duplicated country-year
#' observations. Can be set to \code{none} to do nothing, \code{message} to
#' simply report duplicates, \code{drop} to report and drop duplicates, and
#' \code{return} to return a data frame with only duplicated observations
#' (see also \code{fromLast}).
#' @param fromLast logical indicating if duplication should be considered from
#' the reverse side. Only relevant if \code{duplicates = 'drop'} or
#' \code{duplicates = 'out'}.
#'
#' @return a data frame
#'
#' @examples
#' \dontrun{
#' # Download full data set
#' DDData <- DDGet()
#' }
#'
#' @seealso \code{\link{countrycode}}, \code{\link{CountryID}}
#'
#' @importFrom rio import
#'
#' @export

DDGet <- function(url = 'http://uofi.box.com/shared/static/bba3968d7c3397c024ec.dta',
                  vars = NULL, OutCountryID = 'iso2c',
                  standardCountryName = TRUE,
                  na.rm = TRUE, duplicates = 'message', fromLast = FALSE){
    # Download underlying Polity IV data
    DDData <- import(url)

    # Clean up
    DDData$order <- NULL
    names(DDData)[names(DDData) == "ctryname"] <- "country"

    # Ensure that vars are in the data frame
    if (!is.null(vars)){
        if (!all(vars %in% names(DDData))){
            stop('Specified variables not found in data.')
        }

        Vars <- c('country', 'year', vars)
        DDData <- DDData[, Vars]
    }

    # Include new country ID variable and standardise country names
    DDData <- CountryID(data = DDData, OutCountryID = OutCountryID,
                        timeVar = 'year', duplicates = duplicates,
                        standardCountryName = standardCountryName,
                        fromLast = fromLast)
    # Drop NAs for OutCountryID
    if (isTRUE(na.rm) & duplicates != "return") {
        DDData <- DropNA.psData(data = DDData,
                                timeVar='year',
                                OutCountryID=OutCountryID)
    }
    return(DDData)
}
