#' @title Set value of slot \code{Data}
#'
#' @description \code{setData} assigns a \linkS4class{data.table} to the slot
#' \code{Data} of the input object.
#'
#' @param object Object whose slot \code{Data} is to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{Data}.
#'
#' @return Object with slot Data updated.
#'
#' @examples
#' # We build an empty data.table:
#' library(data.table)
#' Data <- data.table(IDDD = character(0), Value = numeric(0))
#'
#' # We assign this data.table to the slot Data of object ExampleQ:
#' data(ExampleQ)
#' setData(ExampleQ) <- Data
#' ExampleQ
#'
#' @rdname setData
#'
#' @import data.table
#'
#' @export
setGeneric("setData<-", function(object, value){standardGeneric("setData<-")})
#' @rdname setData
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setData",
    signature = c("StQ", "data.table"),
    function(object, value){

        setkeyv(value, setdiff(names(value), 'Value'))
        object@Data <- value
        validObject(object)
        return(object)
    }
)
