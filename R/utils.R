#' Function for creating standardised country names and ID variables
#'
#' Function for creating standardised country names and ID variables based on
#' capabilities from the \code{\link{countrycode}} package. The function also
#' reports if duplicated country IDs have been created and lets the user either
#' drop these or return only duplicated values for inspection.
#' @param data a data frame object
#' @param countryVar character string naming the country.name variable. See
#' \code{\link{countrycode}}.
#' @param OutCountryID character string. The type of country ID you would like
#' to include in the output file along with the country name. See
#' \code{\link{countrycode}} for available options.
#' @param standardCountryName logical. Whether or not to standardise the country
#' names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' @param duplicates character string specifying how to handle duplicated
#' country or country-time observations (for the latter see \code{timeVar}).
#' Can be set to \code{none} to do nothing, \code{message} to simply report
#' duplicates, \code{drop} to report and drop duplicates, and \code{return} to
#' return a data frame with only duplicated observations (see also
#' \code{fromLast}).
#' @param timeVar character string indicating the name of a time variable. For
#' example, country time series often have separate rows based on a \code{year}
#' variable. This is used solely to determine if there are duplicated
#' country-time values.
#' @param fromLast logical indicating if duplication should be considered from
#' the reverse side. Only relevant if \code{duplicates = 'drop'} or
#' \code{duplicates = 'return'}.
#'
#' @seealso {\code{\link{duplicated}}}
#'
#' @import countrycode
#' @export

CountryID <- function(data, countryVar = 'country', OutCountryID = 'iso2c',
                      standardCountryName = TRUE, duplicates = 'message',
                      timeVar = NULL, fromLast = FALSE){
  # Ensure that the countryVar is in the data frame
  if (!(countryVar %in% names(data))) {
    stop(paste('A variable called', countryVar, 'is not in the data frame. \n Please enter a countryVar that is in the data frame.'))
  }

  # Copy data set for duplicates reporting
  OriginalData <- data

  # duplicates argument error message
  dupValues <- c('none', 'message', 'drop', 'return')
  if (!isTRUE(duplicates %in% dupValues)) {
    stop('duplicates must be either "none", "message", "drop", or "return".')
  }

  # Include new country ID variable
  data[, OutCountryID] <- countrycode(data[, countryVar],
                            origin = 'country.name',
                            destination = OutCountryID)

  # Standardise country names
  if (isTRUE(standardCountryName)) {
    data$standardized_country <- countrycode(data[, OutCountryID],
                              origin = OutCountryID,
                              destination = 'country.name')
    data <- MoveFront(data, c(OutCountryID, 'standardized_country', countryVar))
  }
  else if (!isTRUE(standardCountryName)) {
    data <- MoveFront(data, c(OutCountryID, countryVar))
  }

  # Inspect duplicated values
  if (duplicates != 'none') {
    if (is.null(timeVar)) {
      Var1 <- countryVar
      Var2 <- OutCountryID
    }
    else if (!is.null(timeVar)) {
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

    # Get the duplicated values, which are printed out later in message
    country_pieces <- split(TransDupDF[ , Var2], TransDupDF[ , OutCountryID])
    if (is.null(timeVar)) {
      dup_values <- lapply(country_pieces, function(df) unique(df[ , OutCountryID]))
    } else {
      get_dup_values <- function(df) {
        if (length(unique(df[ , timeVar])) == 1) {
          return(paste0(unique(df[, OutCountryID]), ": ", unique(df[, timeVar])))
        } else {
          return(paste0(unique(df[, OutCountryID]), ": ",
                        min(df[, timeVar]), "-", max(df[, timeVar])))
        }
      }
      dup_values <- lapply(country_pieces, get_dup_values)
    }

    # Output
    if (duplicates == 'message') {
      message(paste0(DifDups, ' duplicated values were created when standardising the country ID with ', OutCountryID, '.'))
      if (DifDups > 0) {
        message(paste0('Duplicated values: ', paste(dup_values, collapse = " ; "),
                       '.\nTo inspect duplicated rows set duplicates = "return".\n'))
      }
    }
    else if (duplicates == 'drop') {
      data <- data[!duplicated(data[, Var2], fromLast = fromLast), ]
      message(paste(DifDups, 'duplicated rows were dropped.\n'))
    }
    else if (duplicates == 'return') {
      data <- data[duplicated(data[, Var2], fromLast = fromLast), ]
      message(paste('Only duplicated rows returned.\n'))
    }
  }
  return(data)
}

#' Drop rows from a data frame with missing values in the OutCountryID variable.
#'
#' @param data a data frame object.
#' @param countryVar a character vector naming the variables you would like to
#' have only non-missing (NA) values.
#' @param timeVar variable indicating the time.
#' @param OutCountryID the ID the output country identifier.
#'
#' @source Largely based on \code{DropNA} from the \code{DataCombine} package.
#'
#' @keywords internals
#' @export

DropNA.psData <- function(data, countryVar = 'country', timeVar = NULL,
                          OutCountryID) {
    # Drop if is.NA(OutCountryID)
    DataNoNA <- data[!is.na(data[, OutCountryID]), ]

    # Check which original country values were dropped
    DataNA <- data[is.na(data[, OutCountryID]), ]
    if (is.null(timeVar)) {
        country_pieces <- split(DataNA[, countryVar], DataNA[, countryVar])
        dropped_values <- lapply(country_pieces,
                        function(df) unique(df[ , countryVar]))
    } else {
        country_pieces <- split(DataNA[, c(countryVar, timeVar)],
                            DataNA[, countryVar])
    get_dropped_values <- function(df) {
        if (length(unique(df[ , timeVar])) == 1) {
            return(paste0(unique(df[, countryVar]), ": ", unique(df[, timeVar])))
        } else {
            return(paste0(unique(df[, countryVar]), ": ", min(df[, timeVar]), "-",
                      max(df[, timeVar])))
      }
    }
    dropped_values <- lapply(country_pieces, get_dropped_values)
    }

    TotalDropped <- sum(is.na(data[ , OutCountryID]))

    message(paste0(TotalDropped, " observations dropped based on missing values of the standardised ID variable."))
    message(paste0("The observations that were dropped are: ",
                 paste(dropped_values, collapse = " ; "),
                 ".\nTo keep NA's set na.rm = FALSE\n"))
    return(DataNoNA)
}

#' @source \url{https://github.com/hadley/haven/issues/86#issuecomment-119388845}
#' @noRd

labelDataset <- function(data) {
    correctLabel <- function(x) {

        if (!is.null(attributes(x)$labels)) {
            class(attributes(x)$labels) <- typeof(x)
        }
        return(x)
    }
    for (i in colnames(data)) {
        data[, i] <- correctLabel(data[, i])
    }
    return(data)
}
