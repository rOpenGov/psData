#' Internal function for creating standardised country names and ID variables
#'
#' @param data a data frame object
#' @param countryVar character string naming the country.name variable. See \code{\link{countrycode}}.
#' @param OutCountryID character string. The type of country ID you would like to include in the output file along with the country name. See \code{\link{countrycode}} for available options. 
#'
#' @import countrycode
#' @keywords internals
#' @export

CountryID <- function(data, countryVar = 'country', OutCountryID = 'iso2c'){
  # Include new country ID variable
  data[, OutCountryID] <- countrycode(data[, countryVar], 
                            origin = 'country.name',
                            destination = OutCountryID)

  # Standardise country names
  data <- MoveFront(data, countryVar)
  data <- data[, -1]
  data$country <- countrycode(data[, OutCountryID], 
                            origin = OutCountryID,
                            destination = 'country.name') 
  data <- MoveFront(data, c(OutCountryID, 'country'))
  return(data)
}

#' Drop rows from a data frame with missing values in the OutCountryID variable.
#'
#' @param data a data frame object.
#' @param Var a character vector naming the variables you would like to have only non-missing (NA) values.
#' 
#' @source Largely based on \code{DropNA} from the \code{\link{DataCombine}} package.
#'
#' @keywords internals
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

  message(paste0(TotalDropped, " observations dropped from the data frame based on missing values of the standardised ID variable\n\n"))
  return(DataNoNA)
}