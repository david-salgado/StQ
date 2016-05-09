#' @title Extract parts of an object of class \linkS4class{DDdt}
#'
#' @description \code{[} extracts parts of an object of class \linkS4class{DDdt}.
#'
#' It is indeed the method \code{[} for the class \linkS4class{DDdt}. 
#'
#' @param x Object of class \linkS4class{DDdt}.
#'
#' @param i,j,... Indices corresponding to elements to be extracted. The
#' indices are numeric or character vectors, \code{\link{missing}} or
#' \code{\link{NULL}}. Numeric values are coerced to \code{integer} with
#' \code{\link{as.integer}} (thus truncated to zero).
#'
#' @param drop Included by coherence.
#'
#' @return Object of class \linkS4class{DDdt} with the subsetted input object.
#'
#' @examples
#' \dontrun{
#' data(ExampleStQ)
#' ExampleStQ[IDDD == 'IASSCifraNeg']
#' }
#'
#' @include DDdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
   f = "[",
   signature = c("DDdt"),
   function(x, i, j, ..., drop = TRUE){
    
       mc <- match.call()
       New.x <- x@.Data
       names(New.x) <- x@names
       New.x <- setDT(New.x)
       mc[['x']] <- New.x
       output <- eval(mc, envir = parent.frame())
       return(output)
   }
)
