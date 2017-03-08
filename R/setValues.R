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
<<<<<<< HEAD
#' @include StQ.R dcast_StQ.R getData.R getDD.R setData.R getUnits.R plus.StQ.R
=======
#' @include StQ-class.R dcast_StQ.R getData.R getDD.R setData.R getUnits.R plus.StQ.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @examples
#' library(data.table)
#' data(ExampleStQ)
<<<<<<< HEAD
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
=======
#' newVNCVar <- new(Class = 'VNCdt',
#'                  data.table(IDQual = '', NonIDQual = '', IDDD = 'lTurnover',
#'                             UnitName = '', InFiles = 'FF'))
#' newVNC <- BuildVNC(list(MicroData = newVNCVar))
#' newDD <- new(Class = 'DD',
#'              VarNameCorresp = newVNC, 
#'              MicroData = new(Class = 'DDdt',
#'                              data.table(Variable = 'lTurnover',
#'                                         Sort = 'IDDD',
#'                                         Class = 'numeric',
#'                                         Length = '10',
#'                                         Qual1 = 'ID',
#'                                         ValueRegExp = '')))
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' NewQ <- setValues(object = ExampleStQ,
#'                newDD = newDD,
#'                DDslot = 'MicroData',
#'                Value = expression(log(1 + as.numeric(Turnover))))
#' getValues(NewQ, 'lTurnover')
<<<<<<< HEAD
#' 
#' @include StQ.R DD.R getData.R getDD.R getUnits.R setDD.R setData.R dcast_StQ.R plus.StQ.R getVNC.R
=======
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
<<<<<<< HEAD
=======
#' @include StQ-class.R DD-class.R getData.R getDD.R getUnits.R setDD.R setData.R dcast_StQ.R 
#' plus.StQ.R getVNC.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
        
        if (dim(getVNC(newDD)[[DDslot]])[1] != 1) {

            stop('[StQ::setValues] Only one new variable at a time.')
        }
        
        if (length(DDslot) > 1){
            
            stop('[StQ::setValues] DDslot must be a character vector of length 1.')
        }
        
        
<<<<<<< HEAD
        if (!DDslot %in% names(newDD)){
=======
        if (!DDslot %in% slotNames(newDD)){
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            
            stop('[StQ::setValues] DDslot is not a component of the slot DD of the input object.')
        }

        if (missing(Value)) {

            stop('[StQ::setValues] Value must be a vector of values or an object of class expression.')
        }

        if (!class(Value) %in% c('expression', 'integer', 'numeric', 'character', 'logical')){
            
            stop('[StQ::setValues] Value must be an atomic vector or of class expression.')
        }

        NewData <- getUnits(object)
        
        pasteNA <- function(x, y){
            
<<<<<<< HEAD
            out <- ifelse(is.na(y) | y == '', paste0(x, ''), paste(x, y, sep = "_"))
=======
            out <- ifelse(is.na(y) | y == '', paste0(x, ''), paste(x, y, sep ="_"))
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            return(out)
        }
        newVNC <- getVNC(newDD)[[DDslot]]
        UnitCols <- names(newVNC)[grep('Unit', names(newVNC))]
        NewVarName <- newVNC[['IDDD']]
        for (col in setdiff(names(newVNC), c('IDQual', 'NonIDQual', UnitCols, 'IDDD', 'InFiles',  'VarNameCorresp'))){
            
            NewVarName <- pasteNA(NewVarName, newVNC[[col]])
            
        }

        Data <- getData(object)
        
        if (NewVarName %in% Data[['IDDD']]) {

            setData(object) <- Data[IDDD != NewVarName]
        }

        DD <- getDD(object)
<<<<<<< HEAD

=======
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        newDD <- DD + newDD

        if (class(Value) == 'expression'){

            ExprVariables <- c(all.vars(Value), by)
            ExprVariables <- unlist(lapply(ExprVariables[[1]], function(x){
                
                ifelse(ExtractNames(x) %in% unique(Data[['IDDD']]), x, '')
                
            }))
            ExprVariables <- ExprVariables[ExprVariables != '']

<<<<<<< HEAD
            Data <- getData(object, ExprVariables) 
            newObject <- StQ(Data = Data, DD = newDD)
=======
            Data <- getData(object, ExprVariables, DDslot) 
            newObject <- new(Class = 'StQ', Data = Data, DD = newDD)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8

            Data <- dcast_StQ(newObject)

            if (is.null(by)){

                Data[, Value := eval(Value)]


            } else {

                setkeyv(Data, by)
                Data[, Value := eval(Value), by = eval(by)]
            }

            NewData <- Data[, (ExprVariables) := NULL]
            NewData[, IDDD := NewVarName]

            setcolorder(NewData,
                        c(setdiff(names(NewData), c('Value', 'IDDD')),
                          'IDDD', 'Value'))
<<<<<<< HEAD
            newObject <- StQ(Data = NewData, DD = newDD)
=======
            NewData <- new(Class = 'Datadt', NewData)
            newObject <- new(Class = 'StQ', Data = NewData, DD = newDD)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            output <- object + newObject

        } else {

            NewData[, IDDD := NewVarName]
            NewData[, Value := Value]
            setkeyv(NewData, setdiff(names(NewData), 'Value'))
<<<<<<< HEAD
            NewObject <- StQ(Data = NewData, DD = newDD)
=======
            NewData <- new(Class = 'Datadt', NewData)
            NewObject <- new(Class = 'StQ', Data = NewData, DD = newDD)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            output <- object + NewObject

        }

        return(output)
    }
)

#' @rdname setValues
#'
<<<<<<< HEAD
#' @include StQList.R BuildStQList.R
=======
#' @include StQList-class.R BuildStQList.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
