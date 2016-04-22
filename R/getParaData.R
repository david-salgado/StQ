#' @title Return slot ParaData from a DD object
#'
#' @description \code{getParaData} returns slot \code{ParaData} of the input
#' \linkS4class{DD} object.
#' 
#' @param object a DD Object whose slot \code{ParaData} is queried.
#'
#' @return \linkS4class{data.table} with data from slot \code{ParaData} of the
#' input \linkS4class{DD} object.
#'
#' @examples
#' # An example with data created previosly:
#' data(ExampleDD)
#' getParaData(ExampleDD)
#' 
#' @export
setGeneric("getParaData", function(object){standardGeneric("getParaData")})
#' @rdname getParaData
#' 
#' @include DD-class.R
#' 
#' @export
setMethod(
    f = "getParaData",
    signature = c("DD"),
    function(object){
        
        out <- copy(object@ParaData)
        return(out)
    }
)

