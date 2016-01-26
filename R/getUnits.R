#' @title Extract statistical units from an object of class \linkS4class{StQ}
#'
#' @description This method identifies the IDQual qualifiers in the input object
#' and returns a \linkS4class{data.table} with the values of these qualifiers
#' for each statistical unit.
#'
#' @param object Object of class \linkS4class{StQ}.
#'
#' @return It returns a \code{data.table} with the statistical units in the
#' input object.
#'
#' @examples
#' data(ExampleQ)
#' getUnits(ExampleQ)
#'
#' @export
setGeneric("getUnits", function(object) {standardGeneric("getUnits")})

#' @rdname getUnits
#'
#' @include StQ-class.R getData.R getDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "getUnits",
  signature = c("StQ"),
  function(object){

    DDData <- slot(getDD(object), 'MicroData')
    IDQual <- DDData[Sort == 'IDQual', Variable]
    output <- getData(object)[, IDQual, with = F]
    set2keyv(output, IDQual)
    output <- output[!duplicated(output)]
    return(output)
  }
)
