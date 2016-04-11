#' @title S4 class for the raw key of \linkS4class{rawStQ} objects
#'
#' @description Definition of an S4 class named \code{rawKey} implementing the
#' key of each key-value pair in a \linkS4class{rawStQ} object.
#'
#' The structure of the class \code{rawStQ} comprises a character vector whose
#' components follow precisely defined syntax rules given by the regexp 
#' \code{[A-Za-z]+:[A-Za-z0-9]+(_[A-Za-z]+:[A-Za-z0-9]+)*}.
#' 
#' @slot Data Object of class \linkS4class{data.table} with key-value pair
#' structure. It must have exactly two columns: \code{Key} and \code{Value}.
#' It contains all statistical variables (including some metadata) together with
#' their corresponding values. If \code{Data} is not specified as an input
#' parameter, an empty \linkS4class{data.table} with columns \code{Key} and
#' \code{Value} will be initiated.
#'
#' @slot DD Object of class \linkS4class{DD} with the definition and properties
#' of all variables. If \code{DD} is not specified as an input parameter, an
#' empty \linkS4class{DD} object with columns \code{Variable}, \code{Sort},
#' \code{Class} and \code{Qual1} will be initiated.
#'
#' @examples
#' # An empty key
#' new(Class = 'rawKey')
#' 
#' new(Class = 'rawKey', c('ID:391092SS_Turnover:9834.3_Province:09',
#'                         'ID:asd2SS',
#'                         'ID:1231_Employees:841_NACE:0502'))
#'                         
#' new(Class = 'rawKey', c('ID:391092SS.Turnover:9834.3_Province:09',
#'                         'ID:asd2-SS',
#'                         'ID:1231 Employees:841 NACE:0502'))
#'
#' @import data.table
#'
#' @export
setClass(Class = "rawKey",
         contains = 'character',
         validity = function(object){
             
             numkeySyntax <- "^[A-Za-z]+:[A-Za-z0-9]+(_[A-Za-z]+:[0-9\\.]+)*"
             Validnum <- regexpr(numkeySyntax, object)
             ExactLength <- (attributes(Validnum)$match.length == nchar(object))
             Validnum <- unlist(Validnum) & ExactLength
             
             charkeySyntax <- "^[A-Za-z]+:[A-Za-z0-9]+(_[A-Za-z]+:[A-Za-z0-9]+)*"
             Validchar <- regexpr(charkeySyntax, object)
             ExactLength <- (attributes(Validchar)$match.length == nchar(object))
             Validchar <- unlist(Validchar) & ExactLength
                
             Validany <- Validnum | Validchar
             
             if (!all(Validany)) {
                 
                 indexNotValid <- which(!Validany)
                 InvalidKeys <- object[indexNotValid]
                 stop(paste0('[validity rawKey] Not valid keys detected:\n ',
                             paste0(InvalidKeys, collapse = ',\n')),
                      call. = FALSE)
             }
             
             
             
             return(TRUE)
         }
)
