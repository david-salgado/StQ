#' @title Return the set of Variable Names from the DD slot of an object
#'
#' @description \code{getVariables} returns all Variable names from the slot DD of the input object.  
#' 
#' @param object Object whose Variable names are queried.
#' 
#' @param Sort Character vector with the Sort of Variables ('IDDD', 'IDQual', 'NonIDQual').
#' 
#' @param slots Character vector with the slot names if object class is \linkS4class{DD} or 
#' \linkS4class{StQ}.
#'
#' @return Returns a character vector with the variable names.
#'
#' @examples
#' data(ExampleDD) 
#' getVariables(ExampleDD)
#' getVariables(ExampleDD, Sort = 'IDDD', slots = 'MicroData')
#' getVariables(ExampleStQ, Sort = 'IDDD', slots = 'Aggregates')
#' 
#' 
#' @export
setGeneric("getVariables", function(object, 
                                    Sort = c('IDDD', 'IDQual', 'NonIDQual'), 
                                    slots = setdiff(slotNames(object), 'VarNameCorresp')){
    standardGeneric("getVariables")})

#' @rdname getVariables
#' 
#' @include DDdt-class.R DatadtToDT.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getVariables",
    signature = c("DDdt"),
    function(object, Sort = c('IDDD', 'IDQual', 'NonIDQual'), slots){
        
        if (any(!Sort %in% c('IDDD', 'IDQual', 'NonIDQual'))) stop('[StQ::DDdt] The input parameter Sort can only be composed of "IDQual", "NonIDQual", "IDDD".\n')
        objectDT <- DatadtToDT(object)
        Sort <- intersect(Sort, objectDT[['Sort']])
        setkeyv(objectDT, 'Sort')
        output <- objectDT[Sort][['Variable']]
        return(output)
    }
)

#' @rdname getVariables
#' 
#' @include DD-class.R 
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getVariables",
    signature = c("DD"),
    function(object, 
             Sort = c('IDDD', 'IDQual', 'NonIDQual'), 
             slots = setdiff(slotNames(object), 'VarNameCorresp')){
        
        output <- c()
        for (DDdt in slots) {
            
            aux <- getVariables(slot(object, DDdt), Sort)
            output <- c(output, aux)
        }
        
        output <- unique(output)
        
        return(output)
    }
)


#' @rdname getVariables
#' 
#' @include StQ-class.R getDD.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getVariables",
    signature = c("StQ"),
    function(object, 
             Sort = c('IDDD', 'IDQual', 'NonIDQual'), 
             slots = setdiff(slotNames(getDD(object)), 'VarNameCorresp')){
        
        output <- unique(getVariables(getDD(object), Sort, slots))
        
        return(output)
    }
)
