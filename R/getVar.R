#' @title Return the values of a variable of specified statistical units
#'
#' @description \code{getVar} returns the values of the variable specified as input parameter of
#' those statistical units also specified as an input parameter of an input object.
#'
#' @param object Object of class \linkS4class{StQ}.
#' 
#' @param VarName Character vector with the name of the variable.
#' 
#' @param DDslot Character vector of length 1 with the name of DD slot in which variable in VarName
#' is defined. Its default value is \code{MicroData}.
#' 
#' @param Units \linkS4class{data.table} with the qualifier values identifying each statistical unit
#' in the input object.
#'
#' @return Matrix with the queried values.
#'
#' @examples
#' library(data.table)
#' getVar(ExampleStQ, 'Employees_1.')
#' getVar(ExampleStQ, 'Turnover')
#'
#' @import data.table
#'
#' @export
setGeneric("getVar", 
           function(object, VarName, DDslot = 'MicroData', 
                    Units = getUnits(object, 'MicroData')){
               standardGeneric("getVar")})

#' @rdname getVar
#'
#' @include StQ-class.R getData.R getDD.R DDslotWith.R getNonIDQual.R VarNamesToDD.R VarNamesToDT.R ExtractNames.R
#'
#' @import data.table RepoTime
#' 
#' @export
setMethod(
    f = "getVar",
    signature = c("StQ", "character"),
    function(object, VarName, DDslot = 'MicroData', Units = getUnits(object, 'MicroData')){

        DD <- getDD(object)
        
        if (length(VarName) != 1) {

            stop('[StQ::getVar] Only one variable can be specifed as input.')

        }
        if (length(DDslot) > 1){
            
            stop('[StQ::getVar] DDslot must be a character vector of length 1.')
        }
        
        if (!DDslot %in% slotNames(DD)){
            
            stop('[StQ:getVar] DDslot is not a component of the slot DD of the input object.')
        }
        
        DDVar <- VarNamesToDD(VarName, DD)
        Varslot <- DDslot
        for (DDvarslot in setdiff(slotNames(DDVar), 'VarNameCorresp')){
            
            DDlocal <- slot(DDVar, DDvarslot)
            if(dim(DDlocal)[1] != 0){
                
                Varslot <- DDvarslot
            }
        }
        
        if (Varslot != DDslot){
            
            stop('[StQ::getVar] The variable ', ExtractNames(VarName), ' is not defined in the slot ', DDslot, ' of the slot DD of the input object.')
        }
    
        Varslot <- slot(DD, DDslot)
        Quals <- Varslot[Sort == 'NonIDQual', Variable]
        if (length(Quals) == 0 & VarName != ExtractNames(VarName)){
            
            stop('[StQ::getVar] The variable ', ExtractNames(VarName), ' has not any non-identity qualifiers, so VarName cannot be ', VarName, '.')
        }
        
        if (DDslot != 'MicroData'){
            
            Units <- getUnits(object, DDslot)
        }
        
        Data <- getData(object)
        Data <- merge(Data, Units, by = names(Units), all.y = TRUE)
        
        Var <- VarNamesToDT(VarName, DD)
        for (col in names(Var)){
            
            Data[, (col) := ifelse(is.na(get(col)), Var[[col]], get(col))]
        }

        if (VarName != ExtractNames(VarName)){
            
            Data <- merge(Data, Var, by = names(Var))
        }
        Data <- Data[, c(names(Units), 'Value'), with = F]
        
        DDslot <- slot(getDD(object), DDslot)
        NewVarClass <- DDslot[Variable == ExtractNames(VarName)][['Class']]

        if (dim(Data)[1] == 0) {
            
            newUnits <- copy(Units)
            newUnits[, Unit := '']
            for (col in names(Units)){
                newUnits[, Unit := ifelse(Unit == '', get(col), paste0(Unit, '_', get(col)))]
            }
            
            output <- rep(NA, dim(newUnits)[1])
            output <- as(output, NewVarClass)
            output <- matrix(output, ncol = 1, dimnames = list(newUnits[['Unit']], VarName))
            
            
        } else {
        
            output <- Data[['Value']]
            output <- as(output, NewVarClass)
            Data[, Unit := '']
            for (col in setdiff(names(Data), c('Unit', 'Value'))){
                Data[, Unit := ifelse(Unit == '', get(col), paste0(Unit, '_', get(col)))]
            }

            output <- matrix(output, ncol = 1, dimnames = list(Data[['Unit']], VarName))
            
        }

        return(output)
        
    }
)

#' @rdname getVar
#'
#' @include StQ-class.R getData.R getDD.R VarNamesToDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getVar",
    signature = c("StQList", "character"),
    function(object, VarName, Units = getUnits(object[[length(object)]])){
        
        if (length(VarName) != 1) {
            
            stop('[StQ::getVar] Only one variable can be specifed as input.')
            
        }
        ListofStQ <- object@Data
        output <- lapply(ListofStQ, function(StQ){
            
            out <- getVar(StQ, VarName = VarName, Units = Units)
            return(out)
        })

        output <- Reduce(cbind, output)
        colnames(output) <- getRepo(object@Periods)
        return(output)
    }
)
