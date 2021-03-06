% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setUnits.R
\name{setUnits<-}
\alias{setUnits<-}
\alias{setUnits<-,StQ,data.table-method}
\alias{setUnits<-,StQList,data.table-method}
\title{Return the input object with slot \code{Data} restricted to the specified units}
\usage{
setUnits(object) <- value

\S4method{setUnits}{StQ,data.table}(object) <- value

\S4method{setUnits}{StQList,data.table}(object) <- value
}
\arguments{
\item{object}{Object whose slot \code{Data} is to be restricted.}

\item{value}{\linkS4class{data.table} with the values of the unit qualifiers identifying the 
restricted units.}
}
\value{
Object with the same class as the input object with slot Data restricted to the specified
units.
}
\description{
\code{setUnits} returns the input object with slot \code{Data} restricted to the 
specified units.
}
\examples{
data(ExampleStQ)
NewExampleStQ <- ExampleStQ
Units <- data.table::data.table(ID = stringr::str_pad(1:5, 5, 'left', '0'))
setUnits(NewExampleStQ) <- Units
NewExampleStQ

}
