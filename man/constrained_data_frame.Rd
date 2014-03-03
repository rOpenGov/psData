\name{constrained_data_frame}
\alias{constrained_data_frame}
\title{Create subclasss of \code{psData}}
\usage{
constrained_data_frame(Class, columns = character(), exclusive = FALSE,
  constraints = list(), design = list(), meta = list(),
  where = topenv(parent.frame()))
}
\arguments{
  \item{Class}{\code{character} Name of the new class.}

  \item{columns}{Named \code{character} vector. The names
  are the names of required columns; the values are the
  classes of those columns.  Use \code{ANY} to allow a
  class to be anything.}

  \item{exclusive}{\code{logical} If \code{TRUE}, then the
  data frame can only contain the columns in
  \code{columns}.}

  \item{constraints}{\code{list} of functions. Each
  function should take only one argument, and return
  \code{logical}.}

  \item{where}{\code{environment}. The environment in which
  to store the definition. See \code{\link{setClass}}.}
}
\value{
Invisibly returns a constructor function for the new class.
}
\description{
This function creates a class which directly extends
\code{psData} with the requirement that the slots
(\code{columns}, and \code{exclusive} take specific values.
}
\examples{
Foo <-
  constrained_data_frame("Foo",
                         columns = c(a = "numeric", b = "ANY", c = "factor"),
                         constraints = list(function(x) {x$a > 0}))
showClass("Foo")

# Create a new "Foo" object
foo <- Foo(data.frame(a = runif(3), b = runif(3), c = letters[1:3]))
# this also works
# new("Foo", data.frame(a = runif(3), b = runif(3), c = letters[1:3]))
# works like a normal data.frame
print(foo)
summary(foo)
# errors
try(foo$a <- as.character(foo$a))
try(foo["a", 1] <- -1)
try(foo$a <- NULL)
# errors
try(foo$b <- as.character(foo$b))
try(foo$d <- runif(3))
}

