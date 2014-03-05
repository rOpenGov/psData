#' Get data in all formats read by the default packages
#' 
#' The get_data function requires a URL, a read method ('csv' by default),
#' a panel variable and a time variable. These parameters are easy to pass
#' through little functions like the other get methods in this file.
#' 
#' @param Url character string; the URL for the dataset you would like to download; automatically set by other \code{get} methods (see 'Details'). Note, the URL must end in the file extension for the data type (e.g. \code{.csv}). If it doesn't, then you must specify the data type with \code{read}.
#' @param var.n character string; the main panel variable (column name). Defaults to \code{"country"}; automatically set by other \code{get} methods (see 'Details').
#' @param var.t character string; the main time variable (column name). Defaults to \code{"year"}; automatically set by other \code{get} methods (see 'Details').
#' @param read character string; the format of the data (\code{csv}, \code{dta}, \code{spss}, \code{table} or any other supported \code{read.method} function). Defaults to \code{"csv"}; automatically set by other \code{get} methods (see 'Details').
#' @param vars character vector containing the variables to keep. If \code{vars = NULL} then the entire data set is returned. Note that the main panel and time variables are always returned.
#' @param OutCountryID character string. The type of country ID you would like to include in the output file along with the country name. See \code{\link{countrycode}} for available options. 
#' @param standardCountryName logical. Whether or not to standardise the country names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' @param na.rm logical. Drop observations where \code{OutCountryID} is \code{NA}.
#' @param duplicates character specifying how to handle duplicated country-year observations. Can be set to \code{none} to do nothing, \code{message} to simply report duplicates, \code{drop} to report and drop duplicates, and \code{return} to return a data frame with only duplicated observations (see also \code{fromLast}).
#' @param fromLast logical indicating if duplication should be considered from the reverse side. Only relevant if \code{duplicates = 'drop'} or \code{duplicates = 'out'}.
#' @param quite logical. Whether or not to return messages while getting the data.
#' @param args a list of additional arguments to pass the \code{read} function. Automatically sets SPSS files to import correctly.
#' @param file character string naming an \code{.RData} file to save the output data frame as.
#' 
#' @details The \code{get} method scrapes an online locator (\code{Url}), reads it into a recognized data format (\code{read}), and requires its main panel (\code{var.n}) and time (\code{var.t}) to be imported. The \code{psData} contains the following \code{get} presets:
#' \itemize{
#'   \item \code{\link{get_polity4}}. Polity IV data
#'   \item \code{\link{get_dpi}}. DPI data
#'   \item \code{\link{get_qog}}. QOG data (untested, also produces cross-section standard data frames)
#' }
#' @examples
#' \dontrun{
#' 
#' }
#' 
#' @return a data frame, possibly of \code{\link{psData-class}} (expected output)
#' @importFrom downloader download
#' @keywords get data download
#' @export
get_data <- function(Url = NULL, var.n = "country", var.t = "year", read = "csv", vars = NULL, OutCountryID = "iso2c", standardCountryName = TRUE, na.rm = TRUE, duplicates = 'message', fromLast = FALSE, quite = TRUE, args = NULL, file = NULL, ...) {
  
  if(is.null(Url)) {
    getters = ls("package:psData", pattern = "get_")
    getters = getters[ !getters %in% c("get_data") ]
    return(cat("Available methods:", paste0(getters, collapse = ", "),
               "\n\nSee ?get_data and ?get_methods for usage.\n\n"))
  }

  # Load required packages
  try_require("countrycode")
  try_require("downloader")
  try_require("foreign")
  
  # Create temporary file
  tmpfile <- tempfile()

  # Set basic arguments 
  if (is.null(args)){
    args = list(file = tmpfile)
  } else if (!is.null(args)){
    args = append_list(list(file = tmpfile), args)
  }

  ## Additional arguments for reading the file
  # table args
  if(grepl(".dat$|.txt$|.tab$", Url) | read == "table") {
    read = "read.table"
    if(is.null(unlist(args["sep"])))
      args["sep"] = " "
    if(grepl(".tab$", Url))
      args["sep"] = "\t"
  }
  # csv args
  if(grepl(".csv$", Url) | read == "csv") {
    read = "read.csv"
    if(is.null(unlist(args["sep"])))
      args["sep"] = ","
  }
  if(grepl(".tsv$", Url) | read == "tsv") {
    read = "read.csv"
    if(is.null(unlist(args["sep"])))
      args["sep"] = "\t"
  }
  # stata args
  if(grepl(".dta$", Url) | read == "dta" | read == "Stata") {
    read = "read.dta"
    if(is.null(unlist(args["warn.missing.labels"])))
      args["warn.missing.labels"] = FALSE
  }
  # spss args
  if(grepl(".sav$", Url) | read == "spss" | read == "SPSS") {
    read = "read.spss"
    if(is.null(unlist(args["to.data.frame"])))
      args["to.data.frame"] = TRUE
  }

  # stop if unsupported file type
  acceptable = c('read.table', 'read.csv', 'read.dta', 'read.spss')
  if (!(read %in% acceptable)){
    stop(paste0('Unsupported data type.\n\nMake sure the downloaded data can be can be read by:\n\n', paste(acceptable, collapse = ", ")))
  }

  # Download data
  if (!isTRUE(quite)){
    message("Downloading: ", Url)
  }
  download(Url, tmpfile, mode = "wb", quiet = quite)

  # Read data into a data frame
  data = do.call(read, args = args)

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
            OutCountryID = OutCountryID,
            countryVar = ifelse(is.numeric(var.n), names(data)[var.n], var.n),
            timeVar = ifelse(is.numeric(var.t), names(data)[var.t], var.t),
            duplicates = duplicates, 
            standardCountryName = standardCountryName, fromLast = fromLast)
  
  # Drop NAs for OutCountryID
  if (isTRUE(na.rm)){
    data <- DropNA.psData(data, Var = OutCountryID)
  }

  if (!is.null(file)){
    save(data, file)
  }
  return(data)
}