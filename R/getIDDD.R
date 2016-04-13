#' @title Return the variable names (IDDD) included in the input object
#'
#' @description \code{getIDDD} returns a character vector with all variable 
#' names (IDDD) included in the input object.
#'
#' @param object Object with the IDDD variable identifiers.
#'
#' @return Character vector with all the variable names.
#'
#' @examples
#' getIDDD(ExampleQ)
#'
#' @import data.table
#'
#' @export
setGeneric("getIDDD", function(object){standardGeneric("getIDDD")})

#' @rdname getIDDD
#'
#' @include DD-class.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("DD"),
    function(object){
        
        output <- c()
        slots <- setdiff(slotNames(object), 'VarNameCorresp')
        for (DDslot in slots) {
            
            aux <- slot(object, DDslot)[Sort == 'IDDD', Variable]
            output <- c(output, aux)
        }
        output <- unique(output)    
        return(output)
        
    }
)

#' @rdname getIDDD
#'
#' @include StQ-class.R getDD.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("StQ"),
    function(object){
        
        DD <- getDD(object)
        output <- getIDDD(DD)
        return(output)
        
    }
)

