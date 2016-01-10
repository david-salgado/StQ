#' @title Restrict slot \code{Data} of the input object to specified units
#'
#' @description \code{setUnits} restricts slot \code{Data} to those units
#' specified as input parameter.
#'
#' @param object Objeto whose slot \code{Data} is to be restricted.
#'
#' @param value \linkS4class{data.table} with the values of the unit qualifiers
#' identifying the restricted units.
#'
#' @return Object with the same class as the input object with slot Data
#' restricted to the specified units.
#'
#' @examples
#' library(data.table)
#' data(ExampleQ)
#' NewExampleQ <- ExampleQ
#' Units <- data.table(NOrden = 1:5)
#' setUnits(NewExampleQ) <- Units
#' str(NewExampleQ)
#'
#' @rdname setUnits
#'
#' @export
setGeneric("setUnits<-", function(object, value){standardGeneric("setUnits<-")})
#' @rdname setUnits
#'
#' @include StQ-class.R getData.R setData.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setUnits",
    signature = c("StQ", "data.table"),
    function(object, value){

        Data <- getData(object)
        Data <- merge(Data, value, by = names(value))
        setData(object) <- Data
        return(object)
    }
)
