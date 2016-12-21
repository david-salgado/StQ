#' @title Extract statistical units from an object
#'
#' @description This method identifies the IDQual qualifiers in the input object and returns a
#' \linkS4class{data.table} with the values of these qualifiers for each statistical unit for the
#' slot of the attribute \linkS4class{DD} specified as input.
#'
#' @param object Object of class \linkS4class{StQ} or class \linkS4class{data.table}.
#' 
#' @param DDslot Character vector of length 1 with the name of slot of the attribute
#' \linkS4class{DD}.
#'              
#' @return It returns a \code{data.table} with the statistical units in the input object.
#'
#' @examples
#' data(ExampleStQ)
#' getUnits(ExampleStQ)
#'
#' @export
setGeneric("getUnits", function(object, DDslot = 'MicroData') {standardGeneric("getUnits")})

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
  function(object, DDslot = 'MicroData'){

    DDData <- slot(getDD(object), DDslot)
    IDQual <- DDData[Sort == 'IDQual', Variable]
    output <- getData(object)[, IDQual, with = F]
    setkeyv(output, IDQual)
    output <- output[!duplicated(output, by = key(output))]
    for (IDQ in IDQual){
        
        output <- output[get(IDQ) != '']
        
    }
    return(output)
  }
)

#' @rdname getUnits
#'
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getUnits",
    signature = c("data.table"),
    function(object, DDslot = 'MicroData'){
        
        # For files FL; these files should be reformatted in the repo
        # In Spanish
        if (all(c('LimInf', 'LimSup') %in% names(object))){
            
            IDQual <- setdiff(names(object), 
                              c('Mes', 'NomControl', 'Condicion', 'LimInf', 'LimSup'))
            Units <- object[, IDQual, with = FALSE]
            setkeyv(Units, IDQual)
            Units <- Units[!duplicated(Units, by = key(Units))]
            return(Units)
            
        }
        # In English    
        if (all(c('LowBound', 'UpperBound') %in% names(object))){
            
            IDQual <- setdiff(names(object), 
                              c('Month', 'EditName', 'Condition', 'LowBound', 'UpperBound'))
            Units <- object[, IDQual, with = FALSE]
            setkeyv(Units, IDQual)
            Units <- Units[!duplicated(Units, by = key(Units))]
            return(Units)
        }
        
        # For files FT; these files should be reformatted in the repo
        # In Spanish
        if (all(paste0(c('literal', 'error', 'std', 'quant'), '1') %in% names(object))){
            
            IDQual <- setdiff(names(object),)
        }    
        
    }
)


#' @rdname getUnits
#'
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getUnits",
    signature = c("Datadt"),
    function(object, DDslot = 'MicroData'){
        ColNames.object <- copy(names(object))
        Units <- unique(object[[ColNames.object[1]]])
        Units <- data.table(Units)
        return(Units)
        
    }
)
