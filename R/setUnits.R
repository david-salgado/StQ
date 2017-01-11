#' @title Return the input object with slot \code{Data} restricted to the specified units
#'
#' @description \code{setUnits} returns the input object with slot \code{Data} restricted to the 
#' specified units.
#'
#' @param object Object whose slot \code{Data} is to be restricted.
#'
#' @param value \linkS4class{data.table} with the values of the unit qualifiers identifying the 
#' restricted units.
#'
#' @return Object with the same class as the input object with slot Data restricted to the specified
#' units.
#'
#' @examples
#' library(data.table)
#' library(stringr)
#' data(ExampleStQ)
#' NewExampleStQ <- ExampleStQ
#' Units <- data.table(ID = str_pad(1:5, 5, 'left', '0'))
#' setUnits(NewExampleStQ) <- Units
#' NewExampleStQ
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
        Data <- new(Class = 'Datadt', Data)
        setData(object) <- Data
        return(object)
    }
)
