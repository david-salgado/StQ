#' @title Method to set values of a variable
#'
#' @description \code{setValues} sets the values of a variable in the input object.
#'
#' This method creates a variable with the name \code{VarName} with values specified in the input 
#' \code{Value}. If the variable is already present in the input variable, the method overwrites it.
#'
#' It is also necessary to provide as input parameter an object of class \linkS4class{DD} named 
#' \code{newDD} with the row corresponding to the variable set with this method to be included in
#' the slot \code{DD}. This row is specified as a \linkS4class{data.table} with a single row and 
#' columns Variable, Sort, Class, Qual1-Qualn (as many as so many qualifiers are necessary) and
#' ValueRegExp.
#'
#' Input parameters \code{by} and \code{lag} are optional and specify how to compute the values of 
#' the new variable in some circunstamces.
#'
#' @param object Object in which to include the new variable.
#' 
#' @param newDD \linkS4class{DD} object with the information of the new variable needed for the slot
#'  \code{DD} of the input object.
#'  
#' @param DDslot Character vector of length 1 with the name of the slot of newDD in which the new 
#' variable is defined. Its default value is \code{MicroData}.
#' 
#' @param Value Vector of length the number of statistical units in the input object with the values
#'  of the new variable for each of these units or alternatively an object of class 
#'  \code{\link{call}} with a mathematical formula for the computation of these values.
#'
#' @param by Character vector with the names of the variables specifying those statistical units 
#' groups under which the mathematical formula will be applied. Its default value is \code{NULL}.
#'
#' @param lag Integer vector of length 1 with the lag to use when the input object is of class 
#' \linkS4class{StQList}. Its default value is \code{NULL}. If its value is not \code{NULL}, it is
#' not possible to use an object of class \code{\link{call}} in "Value".
#'
#' @return Object of the same class as the input object with the new variable included.
#'
#' @import data.table
#'
#' @include StQ.R dcast_StQ.R getData.R getDD.R setData.R getUnits.R plus.StQ.R
#'
#' @examples
#' library(data.table)
#' data(ExampleStQ)
#' newVNCVar <- data.table(IDQual = '', NonIDQual = '', IDDD = 'lTurnover',
#'                             UnitName = '', InFiles = 'FF')
#' newVNC <- BuildVNC(list(MicroData = newVNCVar))
#' newDD <- DD(VNC = newVNC, 
#'             MicroData = data.table(Variable = 'lTurnover',
#'                                    Sort = 'IDDD',
#'                                    Class = 'numeric',
#'                                    Length = '10',
#'                                    Qual1 = 'ID',
#'                                    ValueRegExp = ''))
#' NewQ <- setValues(object = ExampleStQ,
#'                newDD = newDD,
#'                DDslot = 'MicroData',
#'                Value = expression(log(1 + as.numeric(Turnover))))
#' getValues(NewQ, 'lTurnover')
#' 
#' @include StQ.R DD.R getData.R getDD.R getUnits.R setDD.R setData.R dcast_StQ.R plus.StQ.R getVNC.R IDDDToUnitNames.R UnitToIDDDNames.R ExtractNames.R
#'
#' @export
setGeneric("setValues", function(object,
                                 newDD,
                                 DDslot = 'MicroData',
                                 Value,
                                 lag = NULL,
                                 by = NULL) {standardGeneric("setValues")})

#' @rdname setValues
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "setValues",
    signature = c("StQ", "DD"),
    function(object,
             newDD,
             DDslot = 'MicroData',
             Value,
             lag = NULL,
             by = NULL){
        
        if (missing(newDD)) {
            
            stop("[StQ::setValues] A new DD object for the new variable is needed.")
        }
        
        if (dim(getVNC(newDD)[[DDslot]][IDDD != ''])[1] != 1) {
            
            stop('[StQ::setValues] Only one new variable at a time.')
        }
        
        if (length(DDslot) > 1){
            
            stop('[StQ::setValues] DDslot must be a character vector of length 1.')
        }
        
        
        if (!DDslot %in% names(newDD)){
            
            stop('[StQ::setValues] DDslot is not a component of the slot DD of the input object.')
        }
        
        if (missing(Value)) {
            
            stop('[StQ::setValues] Value must be a vector of values or an object of class expression.')
        }
        
        if (!class(Value) %in% c('call', 'integer', 'numeric', 'character', 'logical')){
            
            stop('[StQ::setValues] Value must be an atomic vector or of class call')
        }
        
        Data <- getData(object)
        
        if (is.call(Value)){ 
            
            QuotedValue <- deparse(Value[[1]])
            QuotedValue <- gsub('\"', '', QuotedValue)
            subQuotedValue <- gsub('-', 'DasH', QuotedValue)
            subQuotedValue <- gsub('.', 'DoT', subQuotedValue, fixed = TRUE)
            subQuotedValue <- gsub(':', 'CoLoN', subQuotedValue)
            
            ExprVariables <- c(all.vars(parse(text = subQuotedValue)), by)
            ExprVariables <- gsub('DoT', '.', ExprVariables)
            ExprVariables <- gsub('CoLoN', ':', ExprVariables)
            ExprVariables <- gsub('DasH', '-', ExprVariables)
            
            IDDD <- getIDDD(object)
            indexVar <- which(ExtractNames(ExprVariables) %in% IDDD)
            ExprVariables <- ExprVariables[indexVar]
            newVNC <- getVNC(newDD)[[DDslot]][IDDD != '']
            NewUnitName <- newVNC[['UnitName']]
            NewIDDDName <- UnitToIDDDNames(NewUnitName, newDD)
            NewVardt <- VarNamesToDT(NewIDDDName, newDD)
            setkeyv(Data, names(NewVardt))
            DD <- getDD(object)
            oldUnitNames <- IDDDToUnitNames(ExprVariables, DD)
            
            if (ExtractNames(NewIDDDName) %in% DD[[DDslot]][['Variable']]){
                
                DD[[DDslot]] <- DD[[DDslot]][!Variable %in% newDD[[DDslot]][Sort == 'IDDD'][['Variable']]]
                setDD(object) <- DD
                
            }
            newDD <- DD + newDD
            
            newExprVariables <- UnitToIDDDNames(oldUnitNames, newDD)
            newUnitNames <- IDDDToUnitNames(newExprVariables, newDD)
            UnitQuotedValue <- QuotedValue
            for (indexVar in seq(along = newUnitNames)){
                
                UnitQuotedValue <- sub(ExprVariables[indexVar], newUnitNames[indexVar], UnitQuotedValue)
                
            }
            
            newData <- getData(object, unique(ExtractNames(newExprVariables)))
            
            newObject <- StQ(Data = newData, DD = newDD)
            IDQuals <- getIDQual(newObject, DDslot)
            
            newObject.dt <- dcast_StQ(newObject)
            if (length(intersect(names(newObject.dt), newExprVariables)) == length(newExprVariables)){
                
                newData <- newObject.dt[, unique(c(IDQuals, newExprVariables, by)), with = F]
                unitnewExprVariables <- IDDDToUnitNames(newExprVariables, newDD)
                setnames(newData, newExprVariables, unitnewExprVariables)
                
                Value <- parse(text = UnitQuotedValue)
                
                if (is.null(by)){
                    
                    newData[, Value := eval(Value)]
                    
                } else {
                    
                    setkeyv(newData, by)
                    newData[, Value := eval(Value), by = eval(by)]
                    
                }
                
                setnames(newData, unitnewExprVariables, newExprVariables)
                newData[, (newExprVariables) := NULL]
                newData[, IDDD := ExtractNames(NewIDDDName)]
                newData <- merge(newData, NewVardt, by = 'IDDD')
                setcolorder(newData,
                            c(setdiff(names(newData), c('Value', 'IDDD')),
                              'IDDD', 'Value'))
                newObject <- StQ(Data = newData, DD = newDD)
                output <- object + newObject
                
            } else {
                
                setDD(object) <- newDD
                output <- object
            }
            
            
        } else {
            
            newVNC <- getVNC(newDD)[[DDslot]][IDDD != '']
            NewUnitName <- newVNC[['UnitName']]
            NewIDDDName <- UnitToIDDDNames(NewUnitName, newDD)
            NewVardt <- VarNamesToDT(NewIDDDName, newDD)
            
            newData <- getUnits(object)
            newData[, IDDD := ExtractNames(NewIDDDName)]
            newData[, Value := Value]
            setkeyv(newData, 'IDDD')
            
            newData <- merge(newData, NewVardt, by = 'IDDD')
            setcolorder(newData,
                        c(setdiff(names(newData), c('Value', 'IDDD')),
                          'IDDD', 'Value'))
            NewObject <- StQ(Data = newData, DD = newDD)
            output <- object + NewObject
            
        }
        
        return(output)
    }
)

#' @rdname setValues
#'
#' @export
setMethod(
    f = "setValues",
    signature = c("StQList", "DD"),
    function(object,
             newDD,
             DDslot = 'MicroData',
             Value,
             lag = NULL,
             by = NULL){
        
        Object.List <- getData(object)
        mc <- match.call()
        
        output <- lapply(seq(along = Object.List), function(i){
            
            cat(paste0('  [StQ::setValues] Setting values for time period ', names(Object.List)[i], '... '))
            Localmc <- mc
            
            if (!is.null(lag)){
                
                if (i <= lag){
                    
                    LocalOutput <- Object.List[[i]]
                    DD.Aux <- getDD(LocalOutput)
                    DD.Aux <- DD.Aux + newDD
                    setDD(LocalOutput) <- DD.Aux
                } else {
                    
                    LocalOutput.init <- Object.List[[i]]
                    Localmc[['object']] <- Object.List[[(i - lag)]]
                    Localmc[['Value']] <- getValues(Object.List[[(i - lag)]], Value)[[Value]]
                    LocalOutputAux <- eval(Localmc)
                    DD.Aux <- getDD(LocalOutputAux)
                    setDD(LocalOutput.init) <- DD.Aux
                    Var <- newDD[[DDslot]][Sort == 'IDDD'][['Variable']]
                    LocalOutputAux.Data <- getData(LocalOutputAux, c(getIDQual(LocalOutputAux), Var))
                    LocalOutputAux <- StQ(LocalOutputAux.Data, DD.Aux)
                    LocalOutput <- LocalOutput.init + LocalOutputAux
                }
                
            } else {
                
                Localmc[['object']] <- Object.List[[i]]
                LocalOutput <- eval(Localmc)
            }
            cat(' ok.\n')
            return(LocalOutput)
        })
        
        cat('  [StQ::setValues] Building StQList object ...')
        names(output) <- names(Object.List)
        output <- BuildStQList(output)
        cat('   ok.\n')
        return(output)
    }
)
