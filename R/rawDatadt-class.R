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
#' 
#' # An empty rawDatadt object:
#' library(data.table)
<<<<<<< HEAD
#' key <- new(Class = 'rawKey', 
#'           data.table(IDDDKey = c('Turnover', 'Turnover', 'Turnover', 'Turnover'),
#'                      QualKey = c('25641378SS2.0.', '25641378SS1.1.', '25641378SS1.2.', 
#'                                  '25641378SS3.2.')))
#' key <- as(key, 'data.table')
#' rawData <- new(Class = 'rawDatadt', 
#'                cbind(key, Value = c('625000', '23154', '25004', '10512')))
||||||| merged common ancestors
#' key <- new(Class = 'rawKey', 
#'           c('Turnover@@2.@@0.', 
#'             'Turnover@@1.@@1.', 
#'             'Turnover@@1.@@2.', 
#'             'Turnover@@3.@@2.'))
#' rawData <- new(Class = 'rawDatadt', 
#'             data.table(Key = key, Value = c('625000', '23154', '25004', '10512')))
=======
#' new(Class = 'rawDatadt')
>>>>>>> dd5b9b2bd7b111f94c123cf9619690800f3602b6
#'                
#' @include rawKey-class.R
#' 
#' @import data.table
#'
#' @export
setClass(Class = "rawDatadt",
         contains = 'data.table',
<<<<<<< HEAD
         prototype = prototype(data.table(IDDDKey = character(0),
                                          QualKey = character(0),
                                          Value = character(0))),    
||||||| merged common ancestors
         prototype = prototype(data.table(Key = new(Class = 'rawKey'),
                                          Value = character(0))),    
=======
         prototype = prototype(data.table(IDDDKey = character(0),
                                          QualKey = character(0),
                                          Value = character(0))),
>>>>>>> dd5b9b2bd7b111f94c123cf9619690800f3602b6
         validity = function(object){
             

             ColNames <- names(object)
<<<<<<< HEAD
             if (ColNames[1] != 'IDDDKey') {
||||||| merged common ancestors
             if (ColNames[1] != 'Key') {
=======
             if (ColNames[1] != 'IDDDKey') {
                 
                 stop('[validity rawDatadt] The first column of rawDatadt must be IDDDKey.')
             }
             
             if (ColNames[2] != 'QualKey') {
>>>>>>> dd5b9b2bd7b111f94c123cf9619690800f3602b6
                 
<<<<<<< HEAD
                 stop('[validity rawDatadt] The first column of rawDatadt must be IDDDKey.')
||||||| merged common ancestors
                 stop('[validity rawDatadt] The first column of rawDatadt must be Key.')
=======
                 stop('[validity rawDatadt] The second column of rawDatadt must be QualKey.')
>>>>>>> dd5b9b2bd7b111f94c123cf9619690800f3602b6
             }
             
<<<<<<< HEAD
             if (ColNames[2] != 'QualKey') {
||||||| merged common ancestors
             if (ColNames[2] != 'Value') {
=======
             if (ColNames[3] != 'Value') {
>>>>>>> dd5b9b2bd7b111f94c123cf9619690800f3602b6
                 
<<<<<<< HEAD
                 stop('[validity rawDatadt] The second column of rawDatadt must be QualKey.')
             }
             
             if (ColNames[3] != 'Value') {
                 
                 stop('[validity rawDatadt] The third column of rawDatadt must be Value.')
||||||| merged common ancestors
                 stop('[validity rawDatadt] The second column of rawDatadt must be Value.')
=======
                 stop('[validity rawDatadt] The third column of rawDatadt must be Value.')
>>>>>>> dd5b9b2bd7b111f94c123cf9619690800f3602b6
             }
             
             Ncol <- length(ColNames)
             if (Ncol != 3) {
                
                 stop('[validity rawDatadt] Only three columns are allowed in a rawDatadt object.')    
                 
             }
             
             return(TRUE)
         }
)
