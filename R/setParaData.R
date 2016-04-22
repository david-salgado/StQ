#' @title Set value of slot \code{ParaData}
#'
#' @description \code{setParaData} assigns a \linkS4class{data.table} to the slot
#' \code{ParaData} of the input object.
#'
#' @param object Object whose slot \code{ParaData} is to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{ParaData}.
#'
#' @return Object with slot ParaData updated.
#'
#' @examples
#' # We build an empty data.table:
#' library(data.table)
#' ParaData <- data.table(IDDD = character(0), Value = numeric(0))
#'
#' # We assign this data.table to the slot ParaData of object NewExampleDD:
#' data(ExampleDD)
#' NewExampleDD <- ExampleDD
#' setParaData(NewExampleDD) <- ParaData
#' NewExampleDD
#'
#' @rdname setParaData
#'
#' @import data.table
#'
#' @export
setGeneric("setParaData<-", function(object, value){standardGeneric("setParaData<-")})
#' @rdname setParaData
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setParaData",
    signature = c("DD", "data.table"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))
        object@ParaData <- value
        validObject(object)
        return(object)
    }
)
