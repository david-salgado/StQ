#' @title Return the set of Variable Names from the DD slot of an object
#'
#' @description \code{getVariables} returns all Variable names from the slot DD of the input object.  
#' 
#' @param object Object whose Variable names are queried.
#' 
#' @param varSort Character vector with the Sort of Variables ('IDDD', 'IDQual', 'NonIDQual').
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
#' @include DD.R VNC.R StQ.R getDD.R
#' 
#' @export
setGeneric("getVariables", function(object, 
                                    varSort = c('IDDD', 'IDQual', 'NonIDQual'), 
                                    slots = setdiff(names(object), 'VNC')){
    standardGeneric("getVariables")})

#' @rdname getVariables
#' 
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getVariables",
    signature = c("DD"),
    function(object, 
             varSort = c('IDDD', 'IDQual', 'NonIDQual'), 
             slots = setdiff(names(object), 'VNC')){
        
        if (any(!varSort %in% c('IDDD', 'IDQual', 'NonIDQual'))) stop('[StQ::getVariables] The input parameter Sort can only be composed of "IDQual", "NonIDQual", "IDDD".\n')
        output <- c()
        for (DDdt in setdiff(slots, 'VNC')) {
            objectDT <- object[[DDdt]]
            LocalvarSort <- intersect(varSort, objectDT[['Sort']])
            aux <- objectDT[Sort %in% LocalvarSort][['Variable']]
            output <- c(output, aux)
        }
        
        output <- unique(output)
        
        return(output)
    }
)


#' @rdname getVariables
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getVariables",
    signature = c("StQ"),
    function(object, 
             varSort = c('IDDD', 'IDQual', 'NonIDQual'), 
             slots = setdiff(slotNames(getDD(object)), 'VarNameCorresp')){
        
        output <- unique(getVariables(getDD(object), varSort, slots))
        
        return(output)
    }
)
