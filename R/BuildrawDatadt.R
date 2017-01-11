#' @title Constructor of objects of class \linkS4class{rawDatadt}
#'
#' @description This constructor returns an object of class \linkS4class{rawDatadt}. This 
#' constructor builds these objects out of an object of class \linkS4class{rawKey} (with the key of 
#' the key-value pair) and a \code{character} vector (with the values).
#'
#' @param key Object of class \linkS4class{rawKey}.
#' 
#' @param value \code{Character} vector with the values of the variables.
#'
#' @return An object of class \linkS4class{rawDatadt}.
#'
#' @examples
#' library(data.table)
#' key <- new(Class = 'rawKey', 
#'           data.table(IDDDKey = c('Employees', 'Employees', 'RemEmployees', 'Turnover'),
#'                      QualKey = c('25641378SS2.1.1.', '25641378SS1.    ', '25641378SS    1.', 
#'                                  '25641378SS')))
#' value <- c('625', '954', '122', '105124')
#' rawData <- BuildrawDatadt(key, value)
#' 
#' @include rawKey-class.R 
#'
#' @import data.table
#'
#' @export
BuildrawDatadt <- function(key, value){
    
    if (class(key) != 'rawKey'){
        
        stop('[StQ::BuildrawDatadt] The input parameter key must be an object of class rawKey.\n')
    }
    
    DT <- DatadtToDT(key)
    nVar <- dim(DT)[1]
    
    if (nVar != length(value)){
        
        stop('[StQ::BuildrawDatadt] The length of the input parameter value must be', nVar, '.\n')
    }
    
    DT[, Value := value]
    out <- new(Class = 'rawDatadt', DT)
    validObject(out)
    return(out)
}
