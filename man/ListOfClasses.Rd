\docType{class}
\name{ListOfClasses}
\alias{$<-,ListOfClasses-method}
\alias{ListOfClasses}
\alias{ListOfClasses-class}
\alias{[,ListOfClasses-method}
\alias{[<-,ListOfClasses-method}
\alias{[[<-,ListOfClasses-method}
\alias{c,ListOfClasses-method}
\title{List of classes}
\usage{
  ListOfClasses(...)
}
\arguments{
  \item{...}{Passed onto generic functions.}
}
\description{
  An S4 subclass of \code{list} which requires all elements
  of the list to be the same class.
}
\section{Slots}{
  \describe{ \item{\code{.Data}}{Object of class
  \code{list}.} \item{\code{classtype}}{Object of class
  \code{character}. Required classtype for all elements in
  the list.} }
}

\section{Extends}{
  \describe{ \item{\code{list}}{Directly.} }
}

\section{Methods}{
  \describe{ \item{[<-}{\code{signature(x =
  "ListOfClasses")}: ... } \item{[}{\code{signature(x =
  "ListOfClasses")}: ... } \item{[[<-}{\code{signature(x =
  "ListOfClasses")}: ... } \item{c}{\code{signature(x =
  "ListOfClasses")}: ... } }
}
\keyword{classes}

