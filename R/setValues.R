#' @title Method to set values of a variable
#'
#' @description \code{setValues} sets the values of a variable in the input object.
#'
#' This method creates a variable with the name \code{VarName} with values specified in the input 
#' \code{Value}. If the variable is already present in the input variable, the method overwrites it.
#'
#' It is also necessary to provide as input parameter an object of class \linkS4class{DD} named 
#' \code{DDnl} with the row corresponding to the variable set with this method to be included in the
#'  slot \code{DD}. This row is specified as a \linkS4class{DDdt} with a single row and columns 
#'  Variable, Sort, Class, Qual1-Qualn (as many as so many qualifiers are necessary) and ValueRegExp
#' .
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
#'  \code{\link{expression}} with a mathematical formula for the computation of these values.
#'
#' @param by Character vector with the names of the variables specifying those statistical units 
#' groups under which the mathematical formula will be applied.
#'
#' @param lag Integer vector of length 1 with the lag to use when the input object is of class 
#' \linkS4class{StQList}.
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
#' @include StQ.R DD.R getData.R getDD.R getUnits.R setDD.R setData.R dcast_StQ.R plus.StQ.R getVNC.R
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

        if (!class(Value) %in% c('expression', 'integer', 'numeric', 'character', 'logical')){
            
            stop('[StQ::setValues] Value must be an atomic vector or of class expression.')
        }
        
        Data <- getData(object)
        if (is.expression(Value)) {
            
            QuotedValue <- substitute(Value)
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

            #ExprVariables <- unlist(lapply(ExprVariables, function(x){
            
            #    ifelse(ExtractNames(x) %in% unique(Data[['IDDD']]), x, NULL)
            
            #}))

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
                
                UnitQuotedValue <- gsub(ExprVariables[indexVar], newUnitNames[indexVar], UnitQuotedValue)
                    
            }
            
            newData <- getData(object, unique(ExtractNames(newExprVariables)))
            newObject <- StQ(Data = newData, DD = newDD)
            IDQuals <- getIDQual(newObject, DDslot)

            newData <- dcast_StQ(newObject)[, c(IDQuals, newExprVariables, by), with = F]
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
            newData <- merge(newData, NewVardt, all.x = TRUE)
            setcolorder(newData,
                        c(setdiff(names(newData), c('Value', 'IDDD')),
                          'IDDD', 'Value'))
            newObject <- StQ(Data = newData, DD = newDD)
            output <- object + newObject
            
        } else {
            
            newVNC <- getVNC(newDD)[[DDslot]][IDDD != '']
            NewUnitName <- newVNC[['UnitName']]
            NewIDDDName <- UnitToIDDDNames(NewUnitName, newDD)
            NewVardt <- VarNamesToDT(NewIDDDName, newDD)
            
            newData <- getUnits(object)
            newData[, IDDD := ExtractNames(NewIDDDName)]
            newData[, Value := Value]
            setkeyv(newData, 'IDDD')

            newData <- merge(newData, NewVardt, all.x = TRUE)
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
#' @include StQList.R BuildStQList.R
#'
#' @import data.table
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
        
        output <- lapply(object@Data, setValues, newDD, DDslot, Value, lag, by)
        output <- BuildStQList(output)
        
        return(output)
    }
)
