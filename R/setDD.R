#' @title Set value of slot \code{DD}
#'
#' @description \code{setDD} assigns an object of class \linkS4class{DD} to the slot \code{DD} of 
#' the input object.
#'
#' @param object Object whose slot \code{DD} is to be set.
#'
#' @param value Object of class \linkS4class{DD} to be assigned to the input object.
#'
#' @return Object of class \linkS4class{StQ} with slot DD updated.
#'
#' @examples
#' # We build an empty StQ object:
#' library(data.table)
#' Q <- StQ()
#'
#' # We use an DD object previously created:
#' data(ExampleDD)
#' # We assign this DD object to slot DD of the preceding StQ object:
#' setDD(Q) <- ExampleDD
#' str(Q)
#'
#' @rdname setDD
#' 
#' @include StQ.R
#'
#' @export
setGeneric("setDD<-", function(object, value){standardGeneric("setDD<-")})
#' @rdname setDD
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setDD",
    signature = c("StQ", "DD"),
    function(object, value){

        object[['DD']] <- value
        return(object)
    }
)
