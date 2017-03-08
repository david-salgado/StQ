#' @title Extract statistical units from an object
#'
#' @description This method identifies the IDQual qualifiers in the specified slot of the input 
#' object and returns a \linkS4class{data.table} with the values of these qualifiers for each 
#' statistical unit of this slot.
#'
#' @param object Object of class \linkS4class{StQ}.
#' 
#' @param DDslot \code{Character} vector of length 1 with the name of slot of the attribute 
#' \linkS4class{DD}. The default value is \code{MicroData}.
#'              
#' @return It returns a \code{data.table} with the statistical units from the input object.
#'
#' @examples
#' data(ExampleStQ)
#' getUnits(ExampleStQ)
#'
#' @export
setGeneric("getUnits", function(object, DDslot = 'MicroData') {standardGeneric("getUnits")})

#' @rdname getUnits
#'
<<<<<<< HEAD
#' @include StQ.R getData.R getDD.R getIDQual.R
=======
#' @include StQ-class.R getData.R getDD.R DatadtToDT.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "getUnits",
  signature = c("StQ"),
  function(object, DDslot = 'MicroData'){
    
    if (length(DDslot) != 1) stop('[StQ::getUnits] The input parameter DDslot must be of length 1.\n')
<<<<<<< HEAD
    ValidComp <- setdiff(names(getDD(object)), 'VNC')
    NotValidComp <- DDslot[!DDslot %chin% ValidComp]
    if(!DDslot %in% ValidComp) stop(paste0('[StQ::getUnits] The input parameter ', NotValidComp, ' is not a valid DD slot in the input StQ object.\n'))
    IDQual <- getIDQual(object, DDslot)
    output <- getData(object)[, IDQual, with = F]
=======
    ValidComp <- setdiff(slotNames(getDD(object)), 'VarNameCorresp')
    NotValidComp <- DDslot[!DDslot %in% ValidComp]
    if(!DDslot %in% ValidComp) stop(paste0('[StQ::getUnits] The input parameter ', NotValidComp, ' is not a valid DD slot in the input StQ object.\n'))
    IDQual <- getIDQual(object, DDslot)
    output <- DatadtToDT(getData(object))[, IDQual, with = F]
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
    if (dim(output)[1] == 0) return(output)
    setkeyv(output, IDQual)
    output <- output[!duplicated(output, by = key(output))]
    for (IDQ in IDQual){output <- output[get(IDQ) != '']}
    return(output)
  }
)
