#' @title S4 class for the raw key of \linkS4class{rawDatadt} objects.
#'
#' @description Definition of an S4 class named \code{rawKey} implementing the key of each key-value
#'  pair in a \linkS4class{rawDatadt} object.
#' 
#' @examples
#' # An empty key
#' library(data.table)
#' new(Class = 'rawKey')
#' 
#' # An example:
#' library(data.table)
#' key <- new(Class = 'rawKey', 
#'            data.table(IDDDKey = c('Employees', 'Employees', 'RemEmployees', 'Turnover'),
#'                       QualKey = c('25641378SS2.1.1.', '25641378SS1.    ', '25641378SS    1.', 
#'                                   '25641378SS')))
#'                                   
#' @import data.table stringr
#'
#' @export
setClass(Class = "rawKey",
         contains = 'data.table',
         prototype = prototype(data.table(IDDDKey = character(0), 
                                          QualKey = character(0))),
         validity = function(object){
             
             ColNames <- names(object)
             if (ColNames[1] != 'IDDDKey') {
                 
                 stop('[StQ::validity rawKey] The first column of rawKey must be IDDDKey.')
             }
             
             if (ColNames[2] != 'QualKey') {
                 
                 stop('[StQ::validity rawKey] The second column of rawKey must be QualKey.')
             }
             
             Ncol <- length(ColNames)
             if (Ncol != 2) {
                 
                 stop('[StQ::validity rawKey] Only two columns are allowed in a rawKey object.')    
                 
             }
             
             IDDDkeySyntax <- "[A-Za-z0-9]*"
             ValidIDDDKey <- regexpr(IDDDkeySyntax, object[['IDDDKey']])
             ExactLength <- (attributes(ValidIDDDKey)$match.length == nchar(object[['IDDDKey']]))
             ValidIDDDKey <- unlist(ValidIDDDKey) & ExactLength
             
             if (!all(ValidIDDDKey)) {
                 
                 indexNotValid <- which(!ValidIDDDKey)
                 InvalidIDDDKeys <- object[['IDDDKey']]@.Data[indexNotValid]
                 stop(paste0('[StQ::validity rawKey] Not valid IDDDKeys detected:\n ',
                             paste0(InvalidIDDDKeys, collapse = ',\n')),
                      call. = FALSE)
             }
             
             numkeySyntax <- "[A-Za-z0-9]+(^|[A-Za-z0-9.])*"
             Validnum <- regexpr(numkeySyntax, object[['QualKey']])
             ExactLength <- (attributes(Validnum)$match.length == nchar(object[['QualKey']]))
             Validnum <- unlist(Validnum) & ExactLength
             
             charkeySyntax <- "[A-Za-z0-9]+([^A-Za-z0-9._]|[A-Za-z0-9.])*"
             Validchar <- regexpr(charkeySyntax, object[['QualKey']])
             ExactLength <- (attributes(Validchar)$match.length == nchar(object[['QualKey']]))
             Validchar <- unlist(Validchar) & ExactLength

             Validany <- as.logical(Validnum + Validchar)
             Validany[is.na(Validany)] <- TRUE

             if (!all(Validany)) {
                 
                 indexNotValid <- which(!Validany)
                 InvalidKeys <- object[['QualKey']]@.Data[indexNotValid]
                 stop(paste0('[StQ::validity rawKey] Not valid QualKeys detected:\n ',
                             paste0(InvalidKeys, collapse = ',\n')),
                      call. = FALSE)
             }
             
             
             
             return(TRUE)
         }
)
