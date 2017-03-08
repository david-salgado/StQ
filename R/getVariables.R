#' @title Return the set of Variable Names from the DD slot of an object
#'
#' @description \code{getVariables} returns all Variable names from the slot DD of the input object.  
#' 
#' @param object Object whose Variable names are queried.
#' 
<<<<<<< HEAD
#' @param varSort Character vector with the Sort of Variables ('IDDD', 'IDQual', 'NonIDQual').
=======
#' @param Sort Character vector with the Sort of Variables ('IDDD', 'IDQual', 'NonIDQual').
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
<<<<<<< HEAD
#' @include DD.R VNC.R StQ.R getDD.R
#' 
#' @export
setGeneric("getVariables", function(object, 
                                    varSort = c('IDDD', 'IDQual', 'NonIDQual'), 
                                    slots = setdiff(names(object), 'VNC')){
=======
#' 
#' @export
setGeneric("getVariables", function(object, 
                                    Sort = c('IDDD', 'IDQual', 'NonIDQual'), 
                                    slots = setdiff(slotNames(object), 'VarNameCorresp')){
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
    standardGeneric("getVariables")})

#' @rdname getVariables
#' 
<<<<<<< HEAD
=======
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
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getVariables",
    signature = c("DD"),
    function(object, 
<<<<<<< HEAD
             varSort = c('IDDD', 'IDQual', 'NonIDQual'), 
             slots = setdiff(names(object), 'VNC')){
        
        if (any(!varSort %in% c('IDDD', 'IDQual', 'NonIDQual'))) stop('[StQ::getVariables] The input parameter Sort can only be composed of "IDQual", "NonIDQual", "IDDD".\n')
        output <- c()
        for (DDdt in setdiff(slots, 'VNC')) {
            objectDT <- object[[DDdt]]
            LocalvarSort <- intersect(varSort, objectDT[['Sort']])
            aux <- objectDT[Sort %in% LocalvarSort][['Variable']]
=======
             Sort = c('IDDD', 'IDQual', 'NonIDQual'), 
             slots = setdiff(slotNames(object), 'VarNameCorresp')){
        
        output <- c()
        for (DDdt in slots) {
            
            aux <- getVariables(slot(object, DDdt), Sort)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            output <- c(output, aux)
        }
        
        output <- unique(output)
        
        return(output)
    }
)


#' @rdname getVariables
#' 
<<<<<<< HEAD
=======
#' @include StQ-class.R getDD.R
#' 
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @import data.table
#' 
#' @export
setMethod(
    f = "getVariables",
    signature = c("StQ"),
    function(object, 
<<<<<<< HEAD
             varSort = c('IDDD', 'IDQual', 'NonIDQual'), 
             slots = setdiff(slotNames(getDD(object)), 'VarNameCorresp')){
        
        output <- unique(getVariables(getDD(object), varSort, slots))
=======
             Sort = c('IDDD', 'IDQual', 'NonIDQual'), 
             slots = setdiff(slotNames(getDD(object)), 'VarNameCorresp')){
        
        output <- unique(getVariables(getDD(object), Sort, slots))
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        
        return(output)
    }
)
