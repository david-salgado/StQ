#' @title Return the slot of the input \link{DD} object containing the input variable name
#'
#' @description \code{DDslotWith} returns the slot of the input \link{DD} object which 
#'  contains the variable name specified as input parameter \code{VarName}. If the variable is 
#'  present in more than one slot, the input parameter \code{DDslot} is used to choose the correct
#'  slot (with default value MicroData).
#'  
#' @param object Object of class \link{DD}.
#' 
#' @param VarName \code{Character} vector with the name of the variable.
#' 
#' @param DDslot \code{Character} vector of length 1 with the name of DD slot in which variable in 
#' VarName is defined (default value \code{MicroData}).
#'
#' @return Returns an object of class \linkS4class{DDdt} containing the input variable.
#'
#' @examples
#' data(ExampleDD)
#' DDslotWith(ExampleDD, 'Turnover')
#' 
#' @include DD.R VarNamesToDD.R ExtractNames.R
#'
#' @import data.table
#'
#' @export
setGeneric("DDslotWith", 
           function(object, VarName, DDslot = 'MicroData'){standardGeneric("DDslotWith")})

#' @rdname DDslotWith
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "DDslotWith",
    signature = c("DD", "character"),
    function(object, VarName, DDslot = 'MicroData'){
        
        if (length(VarName) != 1) {
            
            stop('[StQ::DDslotWith] Only one variable can be specifed as input.')
            
        }

        DDVar <- VarNamesToDD(VarName, object)

        Varslot <- c()
        for (DDvarslot in setdiff(names(DDVar), 'VNC')){
            
            DDlocal <- DDVar[[DDvarslot]]
            if (dim(DDlocal)[1] != 0) {
                
                Varslot <- c(Varslot, DDvarslot)
            }
        }

        if (!DDslot %in% Varslot){
            
            stop('[StQ::DDslotWith] Variable ', ExtractNames(VarName), ' is not defined in slot ', DDslot, ' of the input object.')
        }
        
        output <- object[[DDslot]]
        
        return(output)
    }
)
