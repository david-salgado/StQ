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
    ColNames.object <- copy(names(object))
    L <- length(ColNames.object)
    H <- length(object[[1]])
    output <- as.data.table(matrix(nrow=H,ncol=L))
    names(output) = ColNames.object
    for (i in 1:L){
      output[[i]] <- object[[i]]
        
    }
    
    return(output)
    
}
