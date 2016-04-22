#' @title Return slot ID from a DD object
#'
#' @description \code{getID} returns slot \code{ID} of the input
#' \linkS4class{DD} object.
#' 
#' @param object a DD Object whose slot \code{ID} is queried.
#'
#' @return \linkS4class{data.table} with data from slot \code{ID} of the
#' input \linkS4class{DD} object.
#'
#' @examples
#' # An example with data created previosly:
#' data(ExampleDD)
#' getID(ExampleDD)
#' 
#' @export
setGeneric("getID", function(object){standardGeneric("getID")})
#' @rdname getID
#' 
#' @include DD-class.R
#' 
#' @export
setMethod(
    f = "getID",
    signature = c("DD"),
    function(object){
        
        out <- copy(object@ID)
        return(out)
    }
)
