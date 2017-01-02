#' @title S4 class for \linkS4class{data.table}s as components of \linkS4class{rawStQ} objects
#'
#' @description Definition of an S4 class named \code{rawDatadt} as a subclass of class
#' \linkS4class{data.table}.
#'
#' The class \code{rawDatadt} is a \linkS4class{data.table} with the following columns:
#'
#' \itemize{
#'  \item \code{IDDDKey}: character vector with the statistical variable identifiers (IDDD).
#'  \item \code{QualKey}: character vector with the sequence of qualifier values (IDQUal + 
#'  NonIDQual).
#'  \item \code{Value}: value.
#' }
#'
#' @examples
#' 
#' # An empty rawDatadt object:
#' library(data.table)
#' new(Class = 'rawDatadt')
#'                
#' @include rawKey-class.R
#' 
#' @import data.table
#'
#' @export
setClass(Class = "rawDatadt",
         contains = 'data.table',
         prototype = prototype(data.table(IDDDKey = character(0),
                                          QualKey = character(0),
                                          Value = character(0))),
         validity = function(object){
             

             ColNames <- names(object)
             if (ColNames[1] != 'IDDDKey') {
                 
                 stop('[validity rawDatadt] The first column of rawDatadt must be IDDDKey.')
             }
             
             if (ColNames[2] != 'QualKey') {
                 
                 stop('[validity rawDatadt] The second column of rawDatadt must be QualKey.')
             }
             
             if (ColNames[3] != 'Value') {
                 
                 stop('[validity rawDatadt] The third column of rawDatadt must be Value.')
             }
             
             Ncol <- length(ColNames)
             if (Ncol != 3) {
                
                 stop('[validity rawDatadt] Only three columns are allowed in a rawDatadt object.')    
                 
             }
             
             return(TRUE)
         }
)
