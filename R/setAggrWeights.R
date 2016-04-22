#' @title Set value of slot \code{AggrWeights}
#'
#' @description \code{setAggrWeights} assigns a \linkS4class{data.table} to the slot
#' \code{AggrWeights} of the input object.
#'
#' @param object Object whose slot \code{AggrWeights} is to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{AggrWeights}.
#'
#' @return Object with slot AggrWeights updated.
#'
#' @examples
#' # We build an empty data.table:
#' library(data.table)
#' AggrWeights <- data.table(IDDD = character(0), Value = numeric(0))
#'
#' # We assign this data.table to the slot AggrWeights of object NewExampleQ:
#' data(ExampleQ)
#' NewExampleQ <- ExampleQ
#' setAggrWeights(NewExampleQ) <- AggrWeights
#' NewExampleQ
#'
#' @rdname setAggrWeights
#'
#' @import data.table
#'
#' @export
setGeneric("setAggrWeights<-", function(object, value){standardGeneric("setAggrWeights<-")})
#' @rdname setAggrWeights
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setAggrWeights",
    signature = c("DD", "data.table"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))
        object@AggrWeights <- value
        validObject(object)
        return(object)
    }
)
