#' @title Return the slot of the input \linkS4class{DD} object containing the input variable names
#'
#' @description \code{DDslotWith} returns the slot of the input \linkS4class{DD} object which 
#'  contains the variable name specified as input parameter \code{VarName}. If the variable is 
#'  present in more than one slot, the input parameter \code{DDslot} is used to choose the correct
#'  slot (with default value MicroData).
#'  
#' @param object Object of class \linkS4class{DD}.
#' 
#' @param VarName \code{Character} vector with the name of the variable.
#' 
#' @param DDslot \code{Character} vector of length 1 with the name of DD slot in which variables in 
#' VarName are defined (default value \code{MicroData}).
#'
#' @return Returns an object of class \linkS4class{DDdt} containing the input variable.
#'
#' @examples
#' data(ExampleDD)
#' DDslotWith(ExampleDD, 'Turnover')
<<<<<<< HEAD
#' 
#' @include DD.R VarNamesToDD.R ExtractNames.R
=======
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table
#'
#' @export
setGeneric("DDslotWith", 
           function(object, VarName, DDslot = 'MicroData'){standardGeneric("DDslotWith")})

#' @rdname DDslotWith
#'
<<<<<<< HEAD
=======
#' @include DD-class.R VarNamesToDD.R ExtractNames.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
<<<<<<< HEAD
        for (DDvarslot in setdiff(names(DDVar), 'VNC')){
            
            DDlocal <- DDVar[[DDvarslot]]
            if (dim(DDlocal)[1] != 0) {
=======
        for (DDvarslot in setdiff(slotNames(DDVar), 'VarNameCorresp')){
            
            DDlocal <- slot(DDVar, DDvarslot)
            if(dim(DDlocal)[1] != 0){
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
                
                Varslot <- c(Varslot, DDvarslot)
            }
        }

        if (!DDslot %in% Varslot){
            
            stop('[StQ::DDslotWith] Variable ', ExtractNames(VarName), ' is not defined in slot ', DDslot, ' of the input object.')
        }
        
<<<<<<< HEAD
        output <- object[[DDslot]]
=======
        output <- slot(object, DDslot)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        
        return(output)
    }
)
