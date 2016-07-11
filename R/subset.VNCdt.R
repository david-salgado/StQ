#' @title Extract parts of an object of class \linkS4class{VNCdt}
#'
#' @description \code{[} extracts parts of an object of class \linkS4class{VNCdt}.
#'
#' It is indeed the method \code{[} for the class \linkS4class{VNCdt}. 
#'
#' @param x Object of class \linkS4class{VNCdt}.
#'
#' @param i,j,... Indices corresponding to elements to be extracted. The indices are numeric or
#' character vectors, \code{\link{missing}} or \code{\link{NULL}}. Numeric values are coerced to
#' \code{integer} with \code{\link{as.integer}} (thus truncated to zero).
#'
#' @param drop Included by coherence.
#'
#' @return Object of class \linkS4class{VNCdt} with the subsetted input object.
#'
#' @examples
#' library(data.table)
#' MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                             NonIDQual = c('', 'Market', ''),
#'                                             IDDD = c(rep('', 2), 'NewOrders'),
#'                                             NumIdEst = c(rep('', 2), '.'),
#'                                             Market = c(rep('', 2), '1.'),
#'                                             Unit1 = c('numidest', '', 'cp09')))
#' MicroData[IDDD == 'NewOrders']
#' 
#'
#' @include VNCdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
   f = "[",
   signature = c("VNCdt"),
   function(x, i, j, ..., drop = TRUE){
    
       mc <- match.call()
       New.x <- x@.Data
       names(New.x) <- x@names
       New.x <- setDT(New.x)
       mc[['x']] <- New.x
       output <- eval(mc, envir = parent.frame())
       
       #output <- new(Class = "VNCdt", output)
       return(output)
   }
)
