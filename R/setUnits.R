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
#' @include StQ.R getData.R setData.R
#'
#' @export
setGeneric("setUnits<-", function(object, value){standardGeneric("setUnits<-")})
#' @rdname setUnits
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setUnits",
    signature = c("StQ", "data.table"),
    function(object, value){

        Data <- getData(object)
        Data <- merge(Data, value, by = names(value), all.y = TRUE)
        colData <- names(Data)
        for (col in colData){
            Data[is.na(get(col)), (col) := '']
        }
        setData(object) <- Data
        return(object)
    }
)
