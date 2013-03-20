#' DataFrameConstr package
#'
#' This package defines two S4 classes 
#'
#' \enumerate{
#' \item `HomogList`: a list in which all elements must be the same class
#' \item `DataFrameConst`: a data frame with optional required columns and  classes, or general constraints.
#' }
#'
#' For each of these classes, there are functions that easily create a subclass.
#'
#' It also defines the most common methods \code{[<-}, \code{[[<-},
#' \code{$<-}, \code{c}, \code{cbind2}, \code{rbind2} for these classes so that the constraints
#' are checked when data in the objects are updated.
#'
#' See \url{https://github.com/jrnold/DataFrameConstr} for example code.
#' 
#' @name DataFramePlus
#' @docType package
NULL

setGeneric("colnames<-")
setGeneric("rownames<-")
