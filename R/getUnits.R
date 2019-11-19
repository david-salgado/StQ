#' @title Extract statistical units from an object
#'
#' @description This method identifies the IDQual qualifiers in the specified slot of the input 
#' object and returns a \linkS4class{data.table} with the values of these qualifiers for each 
#' statistical unit of this slot.
#'
#' @param object Object of class \link{StQ}.
#' 
#' @param DDslot \code{Character} vector of length 1 with the name of slot of the attribute 
#' \link{DD}. The default value is \code{MicroData}.
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
#' @include StQ.R getData.R getDD.R getIDQual.R
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "getUnits",
  signature = c("StQ"),
  function(object, DDslot = 'MicroData'){
    
    if (length(DDslot) != 1) stop('[StQ::getUnits] The input parameter DDslot must be of length 1.\n')
    ValidComp <- setdiff(names(getDD(object)), 'VNC')
    NotValidComp <- DDslot[!DDslot %chin% ValidComp]
    if(!DDslot %in% ValidComp) stop(paste0('[StQ::getUnits] The input parameter ', NotValidComp, ' is not a valid DD slot in the input StQ object.\n'))
    IDQual <- getIDQual(object, DDslot)
    output <- getData(object)
    IDQual <- intersect(IDQual, names(output))
    output <- output[, IDQual, with = F]
    if (dim(output)[1] == 0) return(output)
    setkeyv(output, IDQual)
    output <- output[!duplicated(output, by = key(output))]
    for (IDQ in IDQual) output <- output[get(IDQ) != '']
    return(output)
  }
)
#' @rdname getValues
#'
#'
#' @export
setMethod(
    f = "getUnits",
    signature = c("StQList"),
    function(object, DDslot = 'MicroData'){
        
        if (length(DDslot) != 1) stop('[StQ::getUnits] The input parameter DDslot must be of length 1.\n')
        ListofStQ <- object$Data
        Periods <- RepoTime::getRepo(object$Periods)
        output <- lapply(seq(along = ListofStQ), function(indexStQ){
            
            StQ <- ListofStQ[[indexStQ]]
            out <- getUnits(StQ, DDslot = DDslot)
            out[, Period := Periods[indexStQ]]
            return(out)
        })
        
        output <- rbindlist(output)
        return(output)
    }
)
