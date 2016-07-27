#' @title S4 class for \linkS4class{data.table}s as components of \linkS4class{rawStQ} objects
#'
#' @description Definition of an S4 class named \code{rawDatadt} as a subclass of class
#' \linkS4class{data.table}.
#'
#' The class \code{rawDatadt} is a \linkS4class{data.table} with the following columns:
#'
#' \itemize{
#'  \item \code{IDDDKey}: compund key of class \linkS4class{rawKey} per each value.
#'  \item \code{Value}: value.
#' }
#'
#' @examples
#' library(data.table)
#' key <- new(Class = 'rawKey', 
#'           data.table(IDDDKey = c('Turnover', 'Turnover', 'Turnover', 'Turnover'),
#'                      QualKey = c('25641378SS2.0.', '25641378SS1.1.', '25641378SS1.2.', 
#'                                  '25641378SS3.2.')))
#' key <- as(key, 'data.table')
#' rawData <- new(Class = 'rawDatadt', 
#'                cbind(key, Value = c('625000', '23154', '25004', '10512')))
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
