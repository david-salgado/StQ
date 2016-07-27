#' @title S4 class for the raw key of \linkS4class{rawDatadt} objects.
#'
#' @description Definition of an S4 class named \code{rawKey} implementing the
#' key of each key-value pair in a \linkS4class{rawDatadt} object.
#'
#' The structure of the class \code{rawDatadt} comprises a character vector whose
#' components follow precisely defined syntax rules given by the regexp 
#' \code{[A-Za-z]+:[A-Za-z0-9]+(_[A-Za-z]+:[A-Za-z0-9]+)*}.
#' 
#' @examples
#' # An empty key
#' new(Class = 'rawKey')
#' 
#' # An example:
#' library(data.table)
#' new(Class = 'rawKey', data.table(IDDDKey = c('IASSEmpleo', 'IASSCifraNeg', 'IASSLPCifraNeg'),
#'                                  QualKey = c('391092SS  11','583436SS    ','12314512SS0121')))
#'                         
#'
#' @import data.table stringr
#'
#' @export
setClass(Class = "rawKey",
         contains = 'data.table',
         prototype = prototype(data.table(IDDDKey = character(0), 
                                          QualKey = character(0))),
         validity = function(object){
             
             IDDDkeySyntax <- "[A-Za-z0-9]*"
             ValidIDDDKey <- regexpr(IDDDkeySyntax, object[['IDDDKey']])
             ExactLength <- (attributes(ValidIDDDKey)$match.length == nchar(object[['IDDDKey']]))
             ValidIDDDKey <- unlist(ValidIDDDKey) & ExactLength
             
             if (!all(ValidIDDDKey)) {
                 
                 indexNotValid <- which(!ValidIDDDKey)
                 InvalidIDDDKeys <- object[['IDDDKey']]@.Data[indexNotValid]
                 stop(paste0('[Validity rawKey] Not valid IDDDKeys detected:\n ',
                             paste0(InvalidIDDDKeys, collapse = ',\n')),
                      call. = FALSE)
             }
             
             #numkeySyntax <- "[A-Za-z0-9]+(^|[A-Za-z0-9.)*([0-9.]+(e(+|-)[0-9]+)?)*"
             numkeySyntax <- "[A-Za-z0-9]+(^|[A-Za-z0-9.])*"
             Validnum <- regexpr(numkeySyntax, object[['QualKey']])
             ExactLength <- (attributes(Validnum)$match.length == nchar(object[['QualKey']]))
             Validnum <- unlist(Validnum) & ExactLength
             
             #charkeySyntax <- "[A-Za-z0-9]+([^a-z]|[A-Za-z0-9.])*[A-Za-z0-9]*"
             charkeySyntax <- "[A-Za-z0-9]+([^A-Za-z0-9._]|[A-Za-z0-9.])*"
             Validchar <- regexpr(charkeySyntax, object[['QualKey']])
             ExactLength <- (attributes(Validchar)$match.length == nchar(object[['QualKey']]))
             Validchar <- unlist(Validchar) & ExactLength

             Validany <- as.logical(Validnum + Validchar)
             Validany[is.na(Validany)] <- TRUE

             if (!all(Validany)) {
                 
                 indexNotValid <- which(!Validany)
                 InvalidKeys <- object[['QualKey']]@.Data[indexNotValid]
                 stop(paste0('[Validity rawKey] Not valid QualKeys detected:\n ',
                             paste0(InvalidKeys, collapse = ',\n')),
                      call. = FALSE)
             }
             
             
             
             return(TRUE)
         }
)
