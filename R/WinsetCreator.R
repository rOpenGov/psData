#' Create Winset and Selectorate variables
#' 
#' Creates the winset (W) and a modified version of the selectorate (S) variable from Bueno de Mesquita et al. (2003) using the most recent data available from Polity IV and the Database of Political Institutions.
#' 
#' @param PolityUrl character string. The URL for the Polity IV data set you would like to download. Note: it must be for the SPSS version of the file.
#' @param DpiUrl character string. The URL for the Database of Political Institutions data set you would like to download. Note: the link must be to a Stata formated file.
#' @param OutCountryID character string. The type of country ID you would like to include in the output file along with the country name. See \code{\link{countrycode}} for available options.
#' @param na.rm logical. Drop observations where \code{OutCountryID} is \code{NA}.
#' 
#' @return Returns a data frame with the following columns:
#' \itemize{
#'  \item{\code{iso2c}: }{The ISO two letter country code identifying the country. This can be changed to another country ID system using \code{OutCountryID}}
#'  \item{\code{country}: }{Country names.}
#'  \item{\code{year}: }{The year.}
#'  \item{\code{W}: }{The winset variable. The variable is the same as Bueno de Mesquita (2003) except the military executive component is from DPI, rather than Banks (1996).}
#'  \item{\code{ModS}: }{The modified selectorate variable. Instead of being based on Polity LEGSELEC, which is no longer create, \code{ModS} is based on the Legislative Index of Electoral Competetiveness variable (LIEC) from DPI. No legislature = 1. Unelected legislature = 2. Elected legislature with only 1 candidate = 3. Elected legislatures with 1 party and multiple candidates = 4. All other legislatures = 5. The variable is then standardised between 0 and 1 by subtracting by 1 then dividing by 4. Note: some countries were coded in LIEC as 3.5, so \code{ModS} has 6 levels between 0 and 1.}
#' }
#' 
#' @examples
#' \dontrun{
#' # Create winset data using default options
#' WinsetData <- WinsetCreator()
#' }
#'
#'@source
#' See Bueno de Mesquita Bruce, Alastair Smith, Randolph M. Siverson, and James D. Morrow. 2003. The Logic of Political Survival. Cambridge, MA: MIT Press.
#' 
#' Morrow, J. D., Bueno De Mesquita, B., Siverson, R. M., and Smith, A. 2008. Retesting Selectorate Theory: Separating the Effects of W from Other Elements of Democracy. American Political Science Review, 102(03), 393-400.
#'
#' Polity IV: \url{http://www.systemicpeace.org/}.
#'
#' Modified so that military regime is take from Database of Political Institutions (\url{http://go.worldbank.org/2EAGGLRZ40}), rather than Banks, Arthur S. 1996. Political Handbook of the World. New York: CSA Publications.
#' 
#' @seealso \code{\link{countrycode}}, \code{\link{CountryID}}, \code{\link{DpiGet}}, \code{\link{PolityGet}}
#' @importFrom DataCombine MoveFront
#' @import countrycode
#' @export

WinsetCreator <- function(PolityUrl = 'http://www.systemicpeace.org/inscr/p4v2012.sav', DpiUrl = 'http://bit.ly/1jZ3nmM', OutCountryID = 'iso2c', na.rm = TRUE){  
  # CRAN finess
  xrcomp <- xropen <- parcomp <- military <- NULL

  # Specify component variables to keeps 
  PolityComps <- c('xrcomp', 'xropen', 'parcomp')
  DpiComps <- c('military', 'liec')

  # Download underlying Polity IV data
  PolityData <- PolityGet(url = PolityUrl, vars = PolityComps, 
                          na.rm = na.rm, duplicates = 'drop')
  
  # Download underlying DPI data
  DpiData <- DpiGet(url = DpiUrl, vars = DpiComps, 
                    na.rm = na.rm, duplicates = 'drop')
  
  # Clean for merging
  Comb <- merge(PolityData, DpiData, by = c('iso2c', 'year'))
  
  # Remove observations with missing values for the components of W
  Comb <- subset(Comb, (xrcomp >= 0 & xropen >= 0 & parcomp >= 0 & military >= 0))

  # Recode missing liec variables as NA
  Comb$liec[Comb$liec < 0] <- NA
  
  #### Create W ####

  # Create winset components
  ## 1 if chief executive is not a military officer
  Comb$Wmilitary <- 0
  Comb$Wmilitary[Comb$military == 0] <- 1
  
  ## 1 if chief executive is not selected by heredity or a small group
  Comb$Wxrcomp <- 0
  Comb$Wxrcomp[Comb$xrcomp >= 2] <- 1
  
  ## Chief executive position selected through an open process
  Comb$Wxropen <- 0
  Comb$Wxropen[Comb$xropen > 2] <- 1
  
  ## Relatively stable political groups regularly compete for influence with little coercion
  Comb$Wparcomp <- 0
  Comb$Wparcomp[Comb$parcomp == 5] <- 1
  
  # Combine components
  Comb$WNonStand <- rowSums(Comb[, c('Wmilitary', 'Wxrcomp', 'Wxropen', 'Wparcomp')])
  
  # Standardize between 0 and 1
  Comb$W <- Comb$WNonStand/4
  
  #### Create ModS ####
  Comb$ModS <- Comb$liec
  Comb$ModS[Comb$ModS >= 5] <- 5
  
  # Standardize between 0 and 1
  Comb$ModS <- Comb$ModS - 1
  Comb$ModS <- Comb$ModS/4
  
  # Clean up
  Out <- Comb[, c('iso2c', 'year', 'W', 'ModS')]
  Out$country <- countrycode(Out$iso2c, origin = 'iso2c',
                    destination = 'country.name')
  if (OutCountryID != 'iso2c'){
    Out[, OutCountryID] <- countrycode(Out$iso2c, 
                    origin = 'iso2c', 
                    destination = OutCountryID)
  }
  Out <- Out[, c(OutCountryID, 'country', 'year', 'W', 'ModS')]
  Out <- Out[order(Out[, OutCountryID], Out[, 'year']), ]
  return(Out)
}