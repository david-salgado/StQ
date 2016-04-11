#' @title Return slot Other from a DD object
#'
#' @description \code{getOtherDD} returns slot \code{Other} of the input
#' \linkS4class{DD} object.
#' 
#' @param object a DD Object whose slot \code{Other} is queried.
#'
#' @return \linkS4class{data.table} with data from slot \code{Other} of the
#' input \linkS4class{DD} object.
#'
#' @examples
#' # An example with data created previosly:
#' data(ExampleDD)
#' getOtherDD(ExampleDD)
#' 
#' @export
setGeneric("getOtherDD", function(object){standardGeneric("getOtherDD")})
#' @rdname getAggWeights
#' 
#' @include DD-class.R
#' 
#' @export
setMethod(
    f = "getOtherDD",
    signature = c("DD"),
    function(object){
        
        out <- copy(object@Other)
        return(out)
    }
)
