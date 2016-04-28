#' @title Return the set of Variable Names from an object
#'
#' @description \code{getVariables} returns all Variable names from the input 
#' object.  
#' 
#' @param object Object whose Variable names are queried.
#' 
#' @param Sort Character vector with the Sort of Variables ('IDDD', 'IDQual', 
#' 'NonIDQual').
#' 
#' @param slots Character vector with the slots names if object class is 
#' \linkS4class{DD}.
#'
#' @return In the case of \linkS4class{DD} it returns the Variable columns
#' from each of its slots different from \code{VarNameCorresp} slot.
#'
#' @examples
#' library(data.table)
#' data(DD)
#' 
#' getVariables(DD)
#' getVariables(DD, Sort = 'IDDD', slots = 'MicroData')
#' 
#' 
#' @export
setGeneric("getVariables", function(object, Sort, slots){standardGeneric("getVariables")})

#' @rdname getVariables
#' 
#' @include DDdt-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getVariables",
    signature = c("DDdt"),
    function(object, Sort){
        
        if (missing(Sort)) Sort <- c('IDDD', 'IDQual', 'NonIDQual')
        
        rows <- which(dt[['Sort']] %in% Sort)
        output <- unique(object[rows, Variable])
        output <- output[output != '']
        
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
    function(object, Sort, slots){
        
        if (missing(Sort)) Sort <- c('IDDD', 'IDQual', 'NonIDQual')
        if (missing(slots)) slots <- slotNames(object)
        
        slots <- setdiff(slots, 'VarNameCorresp')
        output <- c()
        for (DDdt in slots) {
            
            IDDD <- getVariables(slot(object,DDdt), Sort)
            output <- c(output, IDDD)
        }
        
        output <- unique(output)
        
        return(output)
    }
)


#' @rdname getVariables
#' 
#' @include StQ-class.R  
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getVariables",
    signature = c("StQ"),
    function(object, Sort, slots){
        
        output <- unique(getVariables(object@DD, Sort, slots))
        
        return(output)
    }
)
