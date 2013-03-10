\name{subclass_list_of_classes}
\alias{subclass_list_of_classes}
\title{Create subclass list of classes}
\usage{
  subclass_list_of_classes(Class, classtype = "ANY",
    where = topenv(parent.frame()))
}
\arguments{
  \item{Class}{\code{character} string name for the class.}

  \item{classtype}{\code{character} The name of the class
  which all elements must inherit from. This is tested with
  \code{is}.}

  \item{where}{Passed to \code{\link{setClass}}.}
}
\description{
  Creates a new subclass of \code{ListOfClasses} which
  requires a specific class.
}

