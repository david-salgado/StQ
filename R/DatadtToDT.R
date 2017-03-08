#' @title Transforms a \linkS4class{Datadt} into a \linkS4class{data.table}
#'
#' @description \code{DatadtToDT} returns a \linkS4class{data.table} with the data and values of the
#'  input \linkS4class{Datadt} object.
#'
#' @param object \linkS4class{Datadt} to be transformed into a \linkS4class{data.table}.
#'
#' @return \linkS4class{data.table}
#' 
#' @details This function is included because of the issues regarding the inheritance of class 
#' \linkS4class{data.table} (see e.g. this \href{http://stackoverflow.com/questions/32206351/how-to-use-data-table-as-super-class-in-s4}{SO entry}).
#' 
#' @examples
#' library(data.table)
#' Ddt <- new(Class = 'Datadt', 
#'             data.table(ID = c('001', '001', '001', '001'), 
#'                        Market = c('0.', '1.', '', ''),
#'                        EsRemuner = c('2.2.','2.1.','',''),
#'                        IDDD = c('Turnover', 'Turnover', 'Province', 'NACE09'),
#'                        Value = c('625000', '23154', '04', '0512')))
#' class(Ddt)                        
#' DT <- DatadtToDT(Ddt)                        
#' class(DT)
#' 
#' data(ExampleDatadt)
#' class(ExampleDatadt)
#' DT <- DatadtToDT(ExampleDatadt)
#' class(DT)
#' 
#' @import data.table
#'
#' @export
DatadtToDT <- function(object){
    
    n <- dim(object)[1]
    output <- data.table(aux = character(n))
    ColNames <- names(object)
    for (col in ColNames) {
        
        output[, (col) := object[[col]]]
        
    }
    output[, aux := NULL]
    
    setcolorder(output, names(object))
    
    return(output[])
    
}
