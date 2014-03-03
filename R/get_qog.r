#' Get Quality of Government datasets in \code{xtdata} format
#'
#' Function to download Quality of Government (QOG) data and load it as a data 
#' frame in R. The result carries an \code{\link{xtdata}} attribute that can be 
#' passed to the \code{\link{xtmerge}} panel data method. Please visit the 
#' QOG Institute website at \url{http://www.qog.pol.gu.se/} for a presentation 
#' of QOG research.
#'
#' @export
#' @param file a filename to save the dataset at. 
#' If set to \code{FALSE} (the default), the function just returns the link 
#' to the dataset. 
#' If set to \code{TRUE}, the server filename of the dataset is used, which 
#' returns either a CSV file if \code{version} is set to \code{std}, or
#' a Stata \code{dta} file otherwise. See 'Details'.
#' @param replace whether to overwrite the dataset even if a file already 
#' exists at the download location. Defaults to \code{FALSE}.
#' @param path a folder path to prepend to the filename and to the codebook
#' if \code{codebook} is not \code{FALSE}.
#' @param version the QOG version: 
#' \code{std} (Standard), \code{soc} (Social Policy), \code{bas} (Basic) 
#' or \code{exp} (Expert). Defaults to \code{std}. See 'Details'.
#' @param format the QOG format, usually \code{cs} for cross-sectional data
#' or \code{ts} for time series in the \code{std} and \code{bas} versions. 
#' See 'Details' for the full list of specifications. Defaults to \code{cs}.
#' @param codebook whether to download the codebook. Calls \code{qogbook} by 
#' passing the \code{codebook}, \code{version} and \code{path} arguments to it, 
#' where \code{codebook} is treated as the filename for the codebook. 
#' Defaults to \code{FALSE}.
#' @param variables a selection of variables to import. \code{ccode} ISO-3N 
#' country codes \code{ccode} and \code{year} identifiers will be forced into
#' the output if relevant.
#' @param years a selection of years to import. Effective only with
#' the \code{ts}, \code{tsl} or \code{ind} formats.
#' @param ... other arguments supplied to the import method, which is 
#' \code{read.csv} by default, 
#' or \code{\link[foreign]{read.dta}} if \code{file} is a Stata \code{dta} dataset,
#' or \code{\link[foreign]{read.spss}} if \code{file} is a SPSS \code{sav} dataset.
#' @details This version of the package handles all four QOG datasets:
#' \tabular{lcl}{
#'  QOG Standard \tab \code{std} \tab 15 May 2013\cr
#'  QOG Social Policy \tab \code{soc} \tab 4 April 2012\cr
#'  QOG Basic \tab \code{bas}): \tab 28 March 2011\cr
#'  QOG Expert Survey \tab \code{exp} \tab 3-6 September 2012\cr
#'  URL: \tab \tab \url{http://www.qog.pol.gu.se}\cr
#' }
#'
#' Each QOG dataset is available in a variety of data formats:
#' 
#' \itemize{
#'   \item QOG datasets \code{std} and \code{bas} 
#'   require format \code{cs} (cross-section) 
#'   or \code{ts} (time series).
#'   \item QOG dataset \code{soc} 
#'   requires format \code{cs}, \code{tsl} (time series, long) 
#'   or \code{tsw} (time series, wide)
#'   \item QOG dataset \code{exp} 
#'   requires format \code{cntry} (country-level) 
#'   or \code{ind} (individual survey)
#' }
#'
#' The QOG Standard series comes in CSV, SPSS and Stata file formats, CVS being
#' the only format that contains numeric codes instead of QOG value labels. 
#' Datasets outside of the QOG Standard series are available only as Stata items 
#' and require that \code{file} ends in \code{.dta} when \code{version} is not 
#' \code{std}. The only exception is dataset \code{csyom}, which automatically
#' sets \code{version} to \code{std} and requires \code{file} to end
#' in \code{.csv}. Filenames with inadequate extensions will be modified to 
#' conform to these expectations if they do not.
#' @seealso \code{\link{qogbook}}, \code{\link[foreign]{read.dta}}, \code{\link[foreign]{read.spss}}
#' @author Francois Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Show URL to QOG Standard cross-section.
#' get_qog()
#' # Show URL to QOG Social Policy time series, long format.
#' get_qog(version = "soc", format = "tsl")
#' ## Download codebook and recent years from QOG Basic cross-section (not run).
#' # QOG = get_qog(file = "qog.cs.txt", version = "bas", format = "cs", 
#' #         years = 2002:2012, codebook = TRUE)
#' ## Download QOG Standard cross-section years of measurement (not run).
#' # QOG = get_qog(tempfile(fileext = ".csv"), format = "csyom")
#' ## Show QOG years of measurement for Gini coefficient (not run).
#' # table(QOG$wdi_gini)
#' @keywords qog

get_qog = function(file = FALSE, replace = FALSE, codebook = FALSE, path = "",
                    version = "std", format = "cs", 
                    variables = NULL, years = NULL, ...) {
  try_require("foreign")
  #
  # currently available
  #
  versions = list(
    std = c("ts", "cs", "csyom"),
    bas = c("ts", "cs"),
    exp = c("ctry", "ind"),
    soc = c("cs", "tsl", "tsw"))
  #
  # correct version
  #
  if(!version %in% names(versions)) {
    stop("Invalid version: use one of ", 
         paste0(names(versions), collapse = ", "))
  }
  #
  # correct format
  #
  if(format == "csyom") {
    version = "std"
    if(!grepl(".csv$|.txt$", file))
      file = gsub("(\\.|\\w){4}$", ".csv", file)
  }
  if(!format %in% unlist(versions[version])) {
    stop("Invalid format: use one of ", 
         paste0(unlist(versions[version]), collapse = ", "))
  }
  #
  # automatic filename
  #
  if(isTRUE(file)) {
    file = paste0("qog_", 
                  version, 
                  "_", 
                  format, 
                  ifelse(version == "std", 
                         paste0("_", "15May13.csv"), 
                         ".dta")
    )
  }
  else {
    if(is.character(file) & version != "std" & !grepl(".dta$", file)) {
      file = gsub("(\\.|\\w){4}$", ".dta", file)
      warning("QOG datasets other than std are available only as Stata files.\n",
              "  The filename that you specified was modified to ", file)      
    }
  }
  if(is.character(path))
    if(nchar(path) > 0) file = paste(path, file, sep = "/")
  #
  # online source
  #
  link = paste0("http://www.qogdata.pol.gu.se/data/",
                ifelse(version == "std", "QoG", "qog"),
                "_", version, "_", format, 
                ifelse(version == "std", paste0("_", "15May13"), ""),
                ifelse(version == "std" & grepl("csv|dta|sav", file), 
                       substring(file, nchar(file) - 3),
                       ".dta")
  )
  if(is.logical(file)) {
    return(link)
  }
  else {
    if(replace || !file.exists(file)) {
      message("Downloading ", link, "...")
      download.file(link, file, mode = "wb", quiet = TRUE)
    }
    else {
      message("Loading from disk...")
    }
  }
  #
  # reader call
  #
  read = "read.csv"
  args = list(file = file, ...)
  # foreign or not
  if(!grepl(".dta$|.sav$", file)) 
    args["sep"] = ";"
  # stata args
  if(grepl(".dta$", file)) {
    read = "read.dta"
    if(is.null(unlist(args["warn.missing.labels"])))
      args["warn.missing.labels"] = FALSE
  }
  # spss args
  if(grepl(".sav$", file)) {
    read = "read.spss"
    if(is.null(unlist(args["to.data.frame"])))
      args["to.data.frame"] = TRUE
  }
  data = do.call(read, args)
  #
  # selected variables
  #
  uids = c("ccode", "ccodealp", "cname", "ccodecow", "ccodewb")
  pids = uids %in% names(data)
  
  # avoid ts bug
  if(grepl("ts|tsl", format) & !any(pids))
    stop("You are trying to load a QOG dataset as time series, but it has no identifier variable.")
  
  # avoid ts bug
  if(grepl("ts|tsl", format) & !"year" %in% names(data))
    stop("You are trying to load a QOG dataset as time series, but it has no year variable.")
  
  if(!is.null(variables)) {
    if(grepl("ts|tsl", format) & !"year" %in% variables) {
      warning("Forcing year identifier into the dataset.")
      variables = c("year", variables)
    }
    if(grepl("std|bas|soc", version) & !"ccode" %in% variables) {
      warning("Forcing ccode identifier into the dataset.")
      variables = c("ccode", variables)
    }
    data = data[, names(data) %in% variables]
  }
  #
  # selected years
  #
  if(!is.null(years) && format %in% c("ts", "tsl", "ind"))
    data = data[data$year %in% years, ]
  #
  # message
  #
  message("Loaded ", file, " (N = ", nrow(data),
          ifelse(format %in% c("ts", "tsl", "ind"),
                 paste0(", ", min(data$year), 
                        "-", max(data$year), 
                        ", T = ", length(unique(data$year))),
                 ""),
          ").")
  #
  # grab codebook
  #
  if(isTRUE(codebook) || grepl(".pdf", codebook))
    qogbook(codebook, version, path, replace)
  #
  # psData class
  #
  pids = uids[pids] 
  if(format == "ts" | format == "tsl") {
    data = psData(data, 
                 design = list(panel = pids[1], format = c(pids[1] = "iso3n"),
                               time = "year", date = c(year = "%Y")), 
                 meta = list(name = "Quality of Government, time series data"))
    )
  }
  #
  # finish line
  #
  return(data)
}
