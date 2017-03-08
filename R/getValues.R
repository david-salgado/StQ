#' @title Return the values of a variable of specified statistical units
#'
#' @description \code{getValues} returns the values of the variable specified as input parameter of
#' those statistical units also specified as an input parameter of the input object.
#'
#' @param object Object of class \linkS4class{StQ} or \linkS4class{StQList}.
#' 
#' @param VarName Character vector with the name of the variable.
#' 
#' @param Units \linkS4class{data.table} with the qualifier values identifying each statistical unit
#' in the input object.
#'
#' @return Returns a \linkS4class{data.table} with the unit qualifier and the corresponding value 
#' for each statistical unit.
#'
#' @examples
#' library(data.table)
#' getValues(ExampleStQ, 'Employees_1.')
<<<<<<< HEAD
#' getValues(ExampleStQ, 'Turnover', Units = data.table(ID = c('00001', '00002')))
#'
#' @include StQ.R getData.R getDD.R DDslotWith.R getNonIDQual.R VarNamesToDD.R VarNamesToDT.R ExtractNames.R
=======
#' getValues(ExampleStQ, 'Turnover')
#'
#' @include StQ-class.R getData.R getDD.R DDslotWith.R getNonIDQual.R VarNamesToDD.R VarNamesToDT.R ExtractNames.R DatadtToDT.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table RepoTime
#'
#' @export
setGeneric("getValues", 
<<<<<<< HEAD
           function(object, VarName, Units = getUnits(object)){standardGeneric("getValues")})
=======
           function(object, VarName, Units = getUnits(object, 'MicroData')){
               
               standardGeneric("getValues")})
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8

#' @rdname getValues
#' 
#' @export
setMethod(
    f = "getValues",
    signature = c("StQ", "character"),
    function(object, VarName, Units){
        
        if (length(VarName) != 1) {
            
            stop('[StQ::getValues] Only one variable can be specifed as input.')
            
        }
        
        DD <- getDD(object)
        VarNameDD <- VarNamesToDD(VarName, DD)
        dimSlotsVarNameDD <- c()
<<<<<<< HEAD
        for (DDvarslot in setdiff(names(VarNameDD), 'VNC')){
            
            DDlocal <- VarNameDD[[DDvarslot]]
            dimSlotsVarNameDD <- c(dimSlotsVarNameDD, dim(DDlocal)[1])
            if (dim(DDlocal)[1] != 0) {VarNameSlot <- DDvarslot}
=======
        for (DDvarslot in setdiff(slotNames(VarNameDD), 'VarNameCorresp')){
            
            DDlocal <- slot(VarNameDD, DDvarslot)
            dimSlotsVarNameDD <- c(dimSlotsVarNameDD, dim(DDlocal)[1])
            if (dim(DDlocal)[1] != 0){VarNameSlot <- DDvarslot}
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        }

        if (all(dimSlotsVarNameDD == 0)) stop(paste0('[StQ::getValues] The variable ', VarName, ' is not present in the DD slot of the input StQ object.\n'))
        if (missing(Units)) Units <- getUnits(object, VarNameSlot)
<<<<<<< HEAD
        DDslot <- VarNameDD[[VarNameSlot]]
=======
        DDslot <- slot(VarNameDD, VarNameSlot)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        VarQuals <- c() 
        QualCols <- names(DDslot)[grep('Qual', names(DDslot))]
        for (col in QualCols){
            
            VarQuals <- c(VarQuals, DDslot[[col]])
        }
<<<<<<< HEAD
        IDQuals <- DD[[VarNameSlot]][Sort == 'IDQual'][['Variable']]
        IDQuals <- unique(IDQuals[IDQuals != ''])
        NonIDQuals <- setdiff(VarQuals, IDQuals)
        IDQuals <- getIDQual(object, VarNameSlot)
        VarNameDT <- VarNamesToDT(VarName, DD)
        output <- getData(object, ExtractNames(VarName))
        output <- merge(output, VarNameDT, by = names(VarNameDT))
=======
        NonIDQuals <- setdiff(VarQuals, getIDQual(slot(DD, VarNameSlot)))
        IDQuals <- getIDQual(object, VarNameSlot)
        output <- DatadtToDT(getData(object, VarName, VarNameSlot))
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        if (dim(output)[1] == 0) stop(paste0('[StQ::getValues] The input parameter ', VarName, ' is not present in the input StQ object.\n'))
        for (NonIDQual in NonIDQuals){
            
            if (length(unique(output[[NonIDQual]])) != 1) stop(paste0('[StQ::getValues] The input parameter ', VarName, ' needs non-unit qualifiers.\n'))
        }
        output <- output[, c(IDQuals, 'Value'), with = FALSE]
        if (!all(names(Units) %in% IDQuals)) stop(paste0('[StQ::getValues] There is no variable ', VarName, ' for this set of units.\n'))
        output <- merge(output, Units, by = names(Units), all.y = TRUE)
        return(output)
    }
)

#' @rdname getValues
#'
<<<<<<< HEAD
=======
#' @include StQ-class.R getData.R getDD.R VarNamesToDD.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @import data.table RepoTime
#'
#' @export
setMethod(
    f = "getValues",
    signature = c("StQList", "character"),
    function(object, VarName, Units){
        
        if (length(VarName) != 1) {
            
            stop('[StQ::getValues] Only one variable can be specifed as input.')
            
        }
        ListofStQ <- object@Data
        output <- lapply(ListofStQ, function(StQ){
            
            out <- getValues(StQ, VarName = VarName, Units = Units)
            return(out)
        })

        output <- Reduce(cbind, output)
        colnames(output) <- getRepo(object@Periods)
        return(output)
    }
)
