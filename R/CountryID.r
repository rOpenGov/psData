#' Function for creating standardised country names and ID variables
#'
#' Function for creating standardised country names and ID variables based on capabilities from the \code{\link{countrycode}} package. The function also reports if duplicated country IDs have been created and lets the user either drop these or return only duplicated values for inspection.
#' @param data a data frame object
#' @param countryVar character string naming the country.name variable. See \code{\link{countrycode}}.
#' @param OutCountryID character string. The type of country ID you would like to include in the output file along with the country name. See \code{\link{countrycode}} for available options. 
#' @param standardCountryName logical. Whether or not to standardise the country names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' @param duplicates character string specifying how to handle duplicated country or country-time observations (for the latter see \code{timeVar}). Can be set to \code{none} to do nothing, \code{message} to simply report duplicates, \code{drop} to report and drop duplicates, and \code{return} to return a data frame with only duplicated observations (see also \code{fromLast}).
#' @param timeVar character string indicating the name of a time variable. For example, country time series often have separate rows based on a \code{year} variable. This is used solely to determine if there are duplicated country-time values.
#' @param fromLast logical indicating if duplication should be considered from the reverse side. Only relevant if \code{duplicates = 'drop'} or \code{duplicates = 'out'}.
#'
#' @seealso {\code{\link{duplicated}}}
#'
#' @import countrycode
#' @export

CountryID <- function(data, countryVar = 'country', OutCountryID = 'iso2c', standardCountryName = TRUE, duplicates = 'message', timeVar = NULL, fromLast = FALSE){
  # Ensure that the countryVar is in the data frame
  if (!(countryVar %in% names(data))){
    stop(paste('A variable called', countryVar, 'is not in the data frame. \n Please enter a countryVar that is in the data frame.'))
  }
  
  # Copy data set for duplicates reporting
  OriginalData <- data

  # duplicates argument error message
  dupValues <- c('none', 'message', 'drop', 'return')
  if (!isTRUE(duplicates %in% dupValues)){
    stop('duplicates must be either "none", "message", "drop", or "return".')
  }

  # Include new country ID variable
  data[, OutCountryID] <- countrycode(data[, countryVar], 
                            origin = 'country.name',
                            destination = OutCountryID)

  # Standardise country names
  if (isTRUE(standardCountryName)){
  data <- MoveFront(data, countryVar)
  data <- data[, -1]
  data$country <- countrycode(data[, OutCountryID], 
                            origin = OutCountryID,
                            destination = 'country.name') 
  data <- MoveFront(data, c(OutCountryID, 'country'))
  }
  else if (!isTRUE(standardCountryName)){
    data <- MoveFront(data = data, OutCountryID)
  }

  # Inspect duplicated values
  if (duplicates != 'none'){
    if (is.null(timeVar)){
      Var1 <- countryVar
      Var2 <- OutCountryID
    }
    else if (!is.null(timeVar)){
      Var1 <- c(countryVar, timeVar)
      Var2 <- c(OutCountryID, timeVar)
    }
    # Find original number of duplicated observations
    OriginDupDF <- OriginalData[duplicated(OriginalData[, Var1]), ]
    OriginCount <- nrow(OriginDupDF)

    # Find duplicated observations in output data set
    TransDupDF <- data[duplicated(data[, Var2]), ]
    TransCount <- nrow(TransDupDF)

    # Difference between the original and transformed data sets
    DifDups <- TransCount - OriginCount

    # Output
    if (duplicates == 'message'){
      message(paste0(DifDups, ' duplicated values were created when standardising the country ID with ', OutCountryID, '.'))
      if (DifDups > 0){paste('\nTo inspect duplicated rows set duplicates = "out".\n')}
    }
    else if (duplicates == 'drop'){
      data <- data[!duplicated(data[, Var2], fromLast = fromLast), ]
      message(paste(DifDups, 'duplicated rows were dropped.\n'))
    }
    else if (duplicates == 'return'){
      data <- data[duplicated(data[, Var2], fromLast = fromLast), ]
      message(paste('Only duplicated rows returned.\n'))
    }
  }
  return(data)
}
