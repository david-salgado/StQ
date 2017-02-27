#' @title Extract parts of an object of class \linkS4class{Datadt}
#'
#' @description \code{[} extracts parts of an object of class \linkS4class{Datadt}.
#'
#' It is indeed the method \code{[} for the class \linkS4class{Datadt}. 
#'
#' @param x Object of class \linkS4class{Datadt}.
#'
#' @param i,j,... Indices corresponding to elements to be extracted. The indices are numeric or
#' character vectors, \code{\link{missing}} or \code{\link{NULL}}. Numeric values are coerced to
#' \code{integer} with \code{\link{as.integer}} (thus truncated to zero).
#'
#' @param drop Included by coherence.
#'
#' @return Object of class \linkS4class{Datadt} with the subsetted input object.
#'
#' @examples
#' library(data.table)
#' Ddt <- new(Class = 'Datadt', data.table(ID = c('001', '001', '001', '001'), 
#'                                         Market = c('2.', '1.', '', ''),
#'                                         IDDD = c('Turnover', 'Turnover', 'Province', 'NACE09'),
#'                                         Value = c('625000', '23154', '04', '0512')))
#' 
#' Ddt[IDDD == 'Turnover']                       
#'
#' @include Datadt-class.R DatadtToDT.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "[",
    signature = c("Datadt"),
    function(x, i, j, ..., drop = TRUE){
        
        #mc <- match.call()
        #New.x <- x@.Data
        #names(New.x) <- x@names
        #New.x <- setDT(New.x)
        #mc[['x']] <- New.x
        #output <- eval(mc, envir = parent.frame())
        mc <- match.call()
        New.x <- DatadtToDT(x)
        mc[['x']] <- New.x
        output <- eval(mc, envir = parent.frame())
        output <- new(Class = 'Datadt', output)
        
        return(output)
    }
)
