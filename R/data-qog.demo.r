#' Quality of Government demo data
#' 
#' Selected variables from the Quality of Government Standard dataset:
#' 
#' \itemize{
#'   \item \code{year}: year of measurement (\code{ts} only)
#'   \item \code{ccode}: country code, numeric (ISO-3N)
#'   \item \code{ccodealp}: country code, alphabetical (ISO-3C)
#'   \item \code{cname}: country name
#'   \item \code{wdi_pop}: population (millions)
#'   \item \code{wdi_gdpc}: GDP per capita (contant dollars)
#'   \item \code{wdi_fr}: fertility rate (average births per woman)
#'   \item \code{chga_hinst}: regime type
#'   \item \code{bl_asy25mf}: average schooling years, both sexes aged 25+
#'   \item \code{bl_asy15f}: average schooling years, females aged 15+
#'   \item \code{bl_asy15m}: average schooling years, males aged 15+
#' }
#' 
#' @seealso \code{\link{find}} to search the index of a QOG dataset
#' @docType data
#' @keywords datasets qog
#' @name qog.demo
#' @aliases qog.ts.demo qog.cs.demo
#' @usage data(qog.ts.demo)
#' data(qog.cs.demo)
#' @format two data frames, cross-sectional (\code{cs}) and time series (\code{ts})
#' @references
#' Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren
#' Holmberg, Bo Rothstein, Petrus Sundin & Richard Svensson. 
#' 2013. \emph{The Quality of Government Dataset}, version 
#' 15May13. 
#' University of Gothenburg: The Quality of Government Institute.
NULL
