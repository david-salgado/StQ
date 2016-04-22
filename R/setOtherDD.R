#' @title Set value of slot \code{Other}
#'
#' @description \code{setOtherDD} assigns a \linkS4class{data.table} to the slot
#' \code{Other} of the input object.
#'
#' @param object Object whose slot \code{Other} is to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{Other}.
#'
#' @return Object with slot Other updated.
#'
#' @examples
#' # We build an empty data.table:
#' library(data.table)
#' Other <- data.table(IDDD = character(0), Value = numeric(0))
#'
#' # We assign this data.table to the slot Other of object NewExampleQ:
#' data(ExampleQ)
#' NewExampleQ <- ExampleQ
#' setOtherDD(NewExampleQ) <- Other
#' NewExampleQ
#'
#' @rdname setOtherDD
#'
#' @import data.table
#'
#' @export
setGeneric("setOtherDD<-", function(object, value){standardGeneric("setOtherDD<-")})
#' @rdname setOtherDD
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setOtherDD",
    signature = c("StQ", "data.table"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))
        object@Other <- value
        validObject(object)
        return(object)
    }
)
