#' @title Return slot Aggregates from a DD object
#'
#' @description \code{getAggr} returns slot \code{Aggregates} of the input
#' \linkS4class{DD} object.
#' 
#' @param object a DD Object whose slot \code{Aggregates} is queried.
#'
#' @return \linkS4class{data.table} with data from slot \code{Aggregates} of the
#' input \linkS4class{DD} object.
#'
#' @examples
#' # An example with data created previosly:
#' data(ExampleDD)
#' getAggr(ExampleDD)
#' 
#' @export
setGeneric("getAggr", function(object){standardGeneric("getAggr")})
#' @rdname getAggr
#' 
#' @include DD-class.R
#' 
#' @export
setMethod(
    f = "getAggr",
    signature = c("DD"),
    function(object){
        
        out <- copy(object@Aggregates)
        return(out)
    }
)
