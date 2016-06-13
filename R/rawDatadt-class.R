#' @title S4 class for \linkS4class{data.table}s as components of 
#' \linkS4class{rawStQ} objects
#'
#' @description Definition of an S4 class named \code{rawDatadt} as a subclass 
#' of class \linkS4class{data.table}.
#'
#' The class \code{rawDatadt} is a \linkS4class{data.table} with the following 
#' columns:
#'
#' \itemize{
#'  \item \code{Key}: compund key of class \linkS4class{rawKey} per each value.
#'  \item \code{Value}: value.
#' }
#'
#' @examples
#' library(data.table)
#' key <- new(Class = 'rawKey', key = c('NOrden', 'CCAA', 'EsRemuner', 'TipoRem', 'RamaCNAE09', 
#'                               'IDRefPond1', 'VarPonder', 'DivisionCNAE09', 'IDRefPond2', 
#'                               'SectorCNAE09', 'GeneralOtrosCNAE09', 'IDRefPond3', 'IDDD'),   
#'                       Data = c('IASS@@9644947400S@@@@@@@@@@@@@@@@@@@@@@@@IASSCifraNeg',
#'                                'IASS@@9644947400S@@@@1@@1@@@@@@@@@@@@@@@@@@IASSEmpleo',
#'                                'IASS@@9644947400S@@03@@@@@@@@@@@@@@@@@@@@@@IASSLPCifraNeg'))
#' Data <- new(Class = 'rawDatadt', 
#'             data.table(Key = key, Value = c('2034120', '414', '0')))
#'                
#' @include rawKey-class.R
#' 
#' @import data.table
#'
#' @export
setClass(Class = "rawDatadt",
         contains = 'data.table',
         prototype = prototype(data.table(Key = new(Class = 'rawKey'),
                                          Value = character(0))),    
         validity = function(object){
             
             ColNames <- names(object)
             if (ColNames[1] != 'Key') {
                 
                 stop('[validity rawDatadt] The first column of rawDatadt must be Key.')
             }
             
             if (ColNames[2] != 'Value') {
                 
                 stop('[validity rawDatadt] The second column of rawDatadt must be Value.')
             }
             
             Ncol <- length(ColNames)
             if (Ncol != 2) {
                
                 stop('[validity rawDatadt] Only two columns are allowed in a rawDatadt object.')    
                 
             }
             
             return(TRUE)
         }
)
