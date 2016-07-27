#' @title Constructor of objects of class \linkS4class{rawDatadt}.
#'
#' @description This constructor returns objects of class \linkS4class{rawDatadt}.
#' The input parameters are an object of class \linkS4class{rawKey} and a character \code{vector}.
#'
#' @param key object of class \linkS4class{rawKey}.
#' 
#' @param value Character vector with the values of the variables in key parameter.
#'
#' @return An object of class \linkS4class{rawDatadt}.
#'
#' @examples
#'
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
        
        stop('[Validity BuildrawDatadt] key must be an object of class rawkey')
    }
    
    DT <- DatadtToDT(key)
    nVar <- dim(DT)[1]
    
    if (nVar != length(value)){
        
        stop('[Validity BuildrawDatadt] Length of vector value must be', nVar)
    }
    
    DT[, Value:= value]
    
    out <- new(Class = 'rawDatadt', DT)
    validObject(out)
    
    return(out)
}
