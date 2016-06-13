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
#' new(Class = 'rawKey', c('IASS@@9644947400S@@@@@@@@@@@@@@@@@@@@@@@@IASSCifraNeg',
#'                         'IASS@@9644947400S@@@@1@@1@@@@@@@@@@@@@@@@@@IASSEmpleo',
#'                         'IASS@@9644947400S@@03@@@@@@@@@@@@@@@@@@@@@@IASSLPCifraNeg'))
#'                         
#'
#' @import data.table stringr
#'
#' @export
setClass(Class = "rawKey",
         contains = 'character',
         validity = function(object){
             
             #numkeySyntax <- "^IDDD:[A-Za-z0-9]+(_[A-Za-z0-9]+:(-?[0-9\\.]+(e(\\+|-)[0-9]+)?))*$"
             keySyntax <- "[A-Za-z]+@@[A-Za-z0-9]*"
             
             Validnum <- regexpr(keySyntax, object)
             #ExactLength <- (attributes(Validnum)$match.length == nchar(object))
             Validnum <- unlist(Validnum)# & ExactLength
             
             # charkeySyntax <- "[A-Za-z]+@@[A-Za-z0-9]"
             # Validchar <- regexpr(charkeySyntax, object)
             # #ExactLength <- (attributes(Validchar)$match.length == nchar(object))
             # Validchar <- unlist(Validchar)# & ExactLength

             # Validany <- as.logical(Validnum * Validchar)
             Validany <- as.logical(Validnum)
             Validany[is.na(Validany)] <- TRUE

             if (!all(Validany)) {
                 
                 indexNotValid <- which(!Validany)
                 InvalidKeys <- object@.Data[indexNotValid]
                 stop(paste0('[Validity rawKey] Not valid keys detected:\n ',
                             paste0(InvalidKeys, collapse = ',\n')),
                      call. = FALSE)
             }
             
             
             
             return(TRUE)
         }
)
