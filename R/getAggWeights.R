#' @title Return slot AggWeights from a DD object
#'
#' @description \code{getAggWeights} returns slot \code{AggWeights} of the input
#' \linkS4class{DD} object.
#' 
#' @param object a DD Object whose slot \code{AggWeights} is queried.
#'
#' @return \linkS4class{data.table} with data from slot \code{AggWeights} of the
#' input \linkS4class{DD} object.
#'
#' @examples
#' # An example with data created previosly:
#' data(ExampleDD)
#' getAggWeights(ExampleDD)
#' 
#' @export
setGeneric("getAggWeights", function(object){standardGeneric("getAggWeights")})
#' @rdname getAggWeights
#' 
#' @include DD-class.R
#' 
#' @export
setMethod(
    f = "getAggWeights",
    signature = c("DD"),
    function(object){
        
        out <- copy(object@AggWeights)
        return(out)
    }
)
