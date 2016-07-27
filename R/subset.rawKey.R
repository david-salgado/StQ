#' @title Extract parts of an object of class \linkS4class{rawKey}
#'
#' @description \code{[} extracts parts of an object of class \linkS4class{rawKey}.
#'
#' It is indeed the method \code{[} for the class \linkS4class{rawKey}. 
#'
#' @param x Object of class \linkS4class{rawKey}.
#'
#' @param i,j,... Indices corresponding to elements to be extracted. The indices are numeric or
#' character vectors, \code{\link{missing}} or \code{\link{NULL}}. Numeric values are coerced to
#' \code{integer} with \code{\link{as.integer}} (thus truncated to zero).
#'
#' @param drop Included by coherence.
#'
#' @return Object of class \linkS4class{rawKey} with the subsetted input object.
#'
#' @examples
#' data(ExamplerawKey)
#' ExamplerawKey[1] 
#'
#' @include rawKey-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
   f = "[",
   signature = c("rawKey"),
   function(x, i, j, ..., drop = TRUE){
    
       mc <- match.call()
       mc[['x']] <- x@.Data
       output <- eval(mc, envir = parent.frame())
       out <- new(Class = "rawKey", output)
       return(out)
   }
)
