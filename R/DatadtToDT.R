#' @title Returns a \linkS4class{data.table} with the data of the input Datadt object.
#'
#' @description \code{DatadtToDT} returns a \linkS4class{data.table} with the data and values of the
#'  input \linkS4class{Datadt} object.
#'
#' @param object Datadt which is desired to transform into a data.table.
#'
#' @return \linkS4class{data.table} 
#'
#' @examples
#' library(data.table)
#' Ddt <- new(Class = 'Datadt', 
#'             data.table(ID = c('001', '001', '001', '001'), 
#'                        IsNatMarket = c('0', '1', '', ''),
#'                        EsRemuner = c('1','0','',''),
#'                        IDDD = c('Turnover', 'Turnover', 'Province', 'NACE09'),
#'                        Value = c('625000', '23154', '04', '0512')))
#'                         
#'                         
#' DatadtToDT(Ddt)                        
#'
#' @import data.table
#'
#' @export
DatadtToDT <-function(object){
    
    n <- dim(object)[1]
    output <- data.table(aux = character(n))
    ColNames <- names(object)
    for (col in ColNames){
        
        output[, col := object[[col]], with = F]
        
    }
    output[, aux := NULL]
    
    setcolorder(output, names(object))
    
    return(output)
    
}
