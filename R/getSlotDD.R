#' @title Return the values of a variable of specified statistical units
#'
#' @description \code{getVar} returns the values of the variable specified as
#' input parameter of those statistical units also specified as an input
#' parameter of an input object.
#'
#' @param object Object of class \linkS4class{StQ}.
#' 
#' @param VarName Character vector with the name of the variable.
#' 
#' @param DDslot Character vector of length 1 with the name of DD slot in which
#' variables in VarName are defined. Its default value is \code{MicroData}.
#' 
#' @param Units \linkS4class{data.table} with the qualifier values identifying
#' each statistical unit in the input object.
#'
#' @return Matrix with the queried values.
#'
#' @examples
#' library(data.table)
#' getVar(ExampleQ, 'Orders_0')
#' getVar(ExampleQ, 'Turnover')
#'
#' @import data.table
#'
#' @export
setGeneric("getSlotDD", function(object, VarName, DDslot = 'MicroData'){standardGeneric("getSlotDD")})

#' @rdname getSlotDD
#'
#' @include DD-class.R VarNamesToDD.R ExtractNames.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "getSlotDD",
    signature = c("DD", "character"),
    function(object, VarName, DDslot = 'MicroData'){
        
        if (length(VarName) != 1) {
            
            stop('[StQ::getSlotDD] Only one variable can be specifed as input.')
            
        }
        
        DDVar <- VarNamesToDD(VarName, object)
        Varslot <- DDslot
        for (DDvarslot in setdiff(slotNames(DDVar), 'VarNameCorresp')){
            
            DDlocal <- slot(DDVar, DDvarslot)
            if(dim(DDlocal)[1] != 0){
                
                Varslot <- DDvarslot
            }
        }
        
        if (Varslot != DDslot){
            
            stop('[DD::getSlotDD] Variable ', ExtractNames(VarName), ' is not defined in the slot ', DDslot, ' of the input object.')
        }
        
        output <- slot(DD, Varslot)
        
        return(output)
    }
)
