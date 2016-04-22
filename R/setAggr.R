#' @title Set value of slot \code{Aggregates}
#'
#' @description \code{setAggr} assigns a \linkS4class{data.table} to the slot
#' \code{Aggregates} of the input object.
#'
#' @param object Object whose slot \code{Aggregates} is to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{Aggregates}.
#'
#' @return Object with slot Aggregates updated.
#'
#' @examples
#' # We build an empty data.table:
#' library(data.table)
#' Aggregates <- data.table(IDDD = character(0), Value = numeric(0))
#'
#' # We assign this data.table to the slot Aggregates of object NewExampleQ:
#' data(ExampleQ)
#' NewExampleQ <- ExampleQ
#' setAggr(NewExampleQ) <- Aggregates
#' NewExampleQ
#'
#' @rdname setAggr
#'
#' @import data.table
#'
#' @export
setGeneric("setAggr<-", function(object, value){standardGeneric("setAggr<-")})
#' @rdname setAggr
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setAggr",
    signature = c("DD", "data.table"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))
        object@Aggregates <- value
        validObject(object)
        return(object)
    }
)
