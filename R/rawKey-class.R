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
#' new(Class = 'rawKey', c('IDDD:IASSEmpleo_Norden:391092SS_EsRemuner:1_TipoRemuner:1',
#'                         'IDDD:IASSCifraNeg_Norden:asd2SS',
#'                         'IDDD:IASSLPCifraNeg_Norden:1231_CCAA:01'))
#'                         
#' new(Class = 'rawKey', c('IDDD:IASSEmpleo.Norden:391092SS.Turnover:9834.3_Province:09',
#'                         'Norden:asd2SS',
#'                         'IDDD:IASSLPCifraNeg Norden:1231 Employees:841 NACE:0502'))
#'
#' @import data.table stringr
#'
#' @export
setClass(Class = "rawKey",
         contains = 'character',
         validity = function(object){
             
             IDDD <- str_detect(object, 'IDDD:')
             if(length(object[IDDD]) != length(object@.Data)){
                 
                 stop('[Validity rawKey] In each element of the caracter vector there must be an element named "IDDD".')
             }
             
             numkeySyntax <- "^[A-Za-z]+:[A-Za-z0-9]+(_[A-Za-z0-9]+:[0-9\\.]+)*"
             Validnum <- regexpr(numkeySyntax, object)
             ExactLength <- (attributes(Validnum)$match.length == nchar(object))
             Validnum <- unlist(Validnum) & ExactLength
             
             charkeySyntax <- "^[A-Za-z]+:[A-Za-z0-9]+(_[A-Za-z0-9]+:[A-Za-z0-9]+)*"
             Validchar <- regexpr(charkeySyntax, object)
             ExactLength <- (attributes(Validchar)$match.length == nchar(object))
             Validchar <- unlist(Validchar) & ExactLength
                
             Validany <- Validnum | Validchar
             
             if (!all(Validany)) {
                 
                 indexNotValid <- which(!Validany)
                 InvalidKeys <- object[indexNotValid]
                 stop(paste0('[Validity rawKey] Not valid keys detected:\n ',
                             paste0(InvalidKeys, collapse = ',\n')),
                      call. = FALSE)
             }
             
             
             
             return(TRUE)
         }
)
