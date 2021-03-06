% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plus.DD.R
\name{+.DD}
\alias{+.DD}
\title{Method \code{+} for the class \link{DD}}
\usage{
\method{+}{DD}(e1, e2)
}
\arguments{
\item{e1}{Object of class \link{DD}.}

\item{e2}{Object of class \link{DD}.}
}
\value{
Object of class \link{DD} resulting from integrating both \link{DD} objects
 in a single \link{DD} object.
}
\description{
\code{+} sums two objects of class \link{DD}. This method overloads the 
operator \link{+} and returns a new object of class \link{DD}.
}
\examples{
library(data.table)
VarList1 <- list(ID = data.table(IDQual = c('ID', rep('', 4)),
                                             NonIDQual = c(rep('', 5)),
                                             IDDD = c('', 'Name', 'Surname', 'PostalAddr', 
                                                      'PhoneNo'),
                                             ID = c('', rep('.', 4)),
                                             UnitName = c('numidest', 'nombre', 'apellidos', 
                                                'direccion', 'telefono'),
                                             InFiles = rep('FI', 5)),
                 MicroData = data.table(IDQual = c('ID', rep('', 2)),
                                                    NonIDQual = c('', 'Market', ''),
                                                    IDDD = c(rep('', 2), 'Turnover'),
                                                    ID = c(rep('', 2), '.'),
                                                    Market = c(rep('', 2), '1'),
                                                    UnitName = c('numidest', '', 'cn05'),
                                                    InFiles = rep('FF, FD, FG', 3)))
VNC1 <- BuildVNC(VarList1)

ID1dt <- data.table(Variable = c('ID', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
                                Sort = c('IDQual', rep('IDDD', 4)),
                                Class = rep('character', 5),
                                Length = c('11', '25', '25', '50', '9'),
                                Qual1 = c('', rep('ID', 4)),
                                ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', 
                                                '(6|9)[0-9]{8}'))
Micro1dt <- data.table(Variable = c('ID', 'Market', 'Turnover'),
                                   Sort = c('IDQual', 'NonIDQual', 'IDDD'),
                                   Class = c(rep('character', 2), 'numeric'),
                                   Length = c('11', '2', '12'),
                                   Qual1 = c('', '', 'ID'),
                                   ValueRegExp = c('[0-9]{9}PP', '(0|1| )', '[0-9]{1,12}'))
Agg1dt <- data.table(Variable = c('Province', 'NACE09', 'Turnover'),
                                 Sort = c(rep('IDQual', 2), 'IDDD'),
                                 Class = c(rep('character', 2), 'numeric'),
                                 Length = c('25', '4', '12'),
                                 Qual1 = c(rep('', 2), 'Province'),
                                 Qual2 = c(rep('', 2), 'NACE09'),
                                 ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])',
                                          '([0-9]{1, 15}| )'))

DD1 <- DD(VNC = VNC1, ID = ID1dt, MicroData = Micro1dt, Aggregates = Agg1dt)

VarList2 <- list(ID = data.table(IDQual = c('ID', rep('', 4)),
                                             NonIDQual = c(rep('', 5)),
                                             IDDD = c('', 'Name', 'Surname', 'PostalAddr', 
                                                      'PhoneNo'),
                                             ID = c('', rep('.', 4)),
                                             UnitName = c('numidest', 'nombre', 'apellidos', 
                                                   'direccion', 'telefono'),
                                             InFiles = rep('FI', 5)),     
                 MicroData = data.table(IDQual = c('ID', rep('', 2)),
                                                   NonIDQual = c('', 'Market', ''),
                                                   IDDD = c(rep('', 2), 'NewOrders'),
                                                   ID = c(rep('', 2), '.'),
                                                   Market = c(rep('', 2), '1.'),
                                                   UnitName = c('numidest', '', 'cp09'),
                                                   InFiles = rep('FF, FD, FG', 3)))
VNC2 <- BuildVNC(VarList2)

ID2dt <- data.table(Variable = c('ID', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
                                Sort = c('IDQual', rep('IDDD', 4)),
                                Class = rep('character', 5),
                                Length = c('11', '25', '25', '50', '9'),
                                Qual1 = c('', rep('ID', 4)),
                                ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}'))
Micro2dt <- data.table(Variable = c('ID', 'Market', 'NewOrders'),
                                   Sort = c('IDQual', 'NonIDQual', 'IDDD'),
                                   Class = c(rep('character', 2), 'numeric'),
                                   Length = c('11', '2', '7'),
                                   Qual1 = c(rep('', 2), 'ID'),
                                   ValueRegExp = c('[0-9]{9}PP', '(0|1| )', 
                                                   '([0-9]{1, 10}| )'))
Agg2dt <- data.table(Variable = c('Province', 'NACE09', 'NewOrders'),
                                 Sort = c(rep('IDQual', 2), 'IDDD'),
                                 Class = c(rep('character', 2), 'numeric'),
                                 Length = c('25', '4', '7'),
                                 Qual1 = c(rep('', 2), 'Province'),
                                 Qual2 = c(rep('', 2), 'NACE09'),
                                 ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])',
                                             '([0-9]{1, 15}| )'))

DD2 <- DD(VNC = VNC2, ID = ID2dt, MicroData = Micro2dt, Aggregates = Agg2dt)

DD1 + DD2

}
