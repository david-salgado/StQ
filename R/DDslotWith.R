#' @title Return the slot of the input \linkS4class{DD} object containing the input variable names
#'
#' @description \code{DDslotWith} returns the slot of the input \linkS4class{DD} object which 
#'  contains the variable name specified as input parameter \code{VarName}. If the variable is 
#'  present in more than one slot, the input parameter \code{DDslot} is used to choose the correct
#'  slot; otherwise an error is triggered.
#'  
#' @param object Object of class \linkS4class{DD}.
#' 
#' @param VarName Character vector with the name of the variable.
#' 
#' @param DDslot Character vector of length 1 with the name of DD slot in which
#' variables in VarName are defined. Its default value is \code{MicroData}.
#'
#' @return Matrix with the queried values.
#'
#' @examples
#' data(ExampleDD)
#' DDslotWith(ExampleDD, 'Turnover')
#'
#' @import data.table
#'
#' @export
setGeneric("DDslotWith", 
           function(object, VarName, DDslot = 'MicroData'){standardGeneric("DDslotWith")})

#' @rdname DDslotWith
#'
#' @include DD-class.R VarNamesToDD.R ExtractNames.R
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
        for (DDvarslot in setdiff(slotNames(DDVar), 'VarNameCorresp')){
            
            DDlocal <- slot(DDVar, DDvarslot)
            if(dim(DDlocal)[1] != 0){
                
                Varslot <- c(Varslot, DDvarslot)
            }
        }

        if (!DDslot %in% Varslot){
            
            stop('[DD::DDslotWith] Variable ', ExtractNames(VarName), ' is not defined in slot ', DDslot, ' of the input object.')
        }
        
        output <- slot(object, DDslot)
        
        return(output)
    }
)
