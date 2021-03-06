% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plus.StQ.R
\name{+.StQ}
\alias{+.StQ}
\title{Method \code{+} for the class \link{StQ}}
\usage{
\method{+}{StQ}(e1, e2)
}
\arguments{
\item{e1}{Object of class \code{StQ}.}

\item{e2}{Object of class \code{StQ}.}
}
\value{
Object of class \code{StQ} with the join of both slots \code{DD} and
\code{Data}.
}
\description{
\code{+} joins two objects of class \link{StQ} in a single object of the same
 class.

This method overloads the operator \code{\link{+}} and builds a new \link{StQ} object 
combining both input objects.
}
\examples{
# We build a trivial data set:
library(data.table)

Data1 <- data.table(ID = c('002', '003', '004'),
                    IDDD = c('NewOrders', 'NewOrders', 'NewOrders'),
                    Value = c(32150, 12574, 23896))
VNC1 <- data.table(IDQual = c('ID', ''),
                   NonIDQual = c('', ''),
                   IDDD = c('', 'NewOrders'),
                   ID = c('', '.'),
                   UnitName = c('numidest', 'cp01'),
                   InFiles = rep('FF', 2))
VNC1 <- BuildVNC(list(MicroData = VNC1))

MicroDD1 <- data.table(Variable = c('ID', 'NewOrders'),
                       Sort = c('IDQual', 'IDDD'),
                       Class = c('character', 'numeric'),
                       Length = c('11', '7'),
                       Qual1 = c('', 'ID'),
                       ValueRegExp = c('[0-9]{9}PP', '([0-9]{1, 10}| )'))
DD1 <- DD(VNC = VNC1, MicroData = MicroDD1)

# We build the StQ object, and join it with another previously created in a
#  single object:
data(ExampleStQ)  
Q1 <- StQ(Data = Data1, DD = DD1)
Q2 <- ExampleStQ
Q1 + Q2


}
