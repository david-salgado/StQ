#' @title Method to set values of a variable.
#'
#' @description \code{setVar} sets the values of a variable in the input object.
#'
#' This method creates a variable with the name \code{VarName} with values
#' specified in the input \code{Value}. If the variable is already present in
#' the input variable, the method overwrites it.
#'
#' It is also necessary to provide as input parameter an object of class
#' \linkS4class{DD} named \code{DDnl} with the row corresponding to the variable
#' set with this method to be included in the slot \code{DD}. This row is
#' specified as a \linkS4class{DDdt} with a single row and columns Variable,
#' Sort, Class, Qual1-Qualn (as many as so many qualifiers are necessary) and
#' ValueRegExp.
#'
#' Input parameters \code{by} and \code{lag} are optional and specify how to
#' compute the values of the new variable in some circunstamces.
#'
#' @param object Object in which to include the new variable.
#' 
#' @param newDD \linkS4class{DD} object with the information of the new variable
#'  needed for the slot \code{DD} of the input object.
#'
#' @param Value Vector of length the number of statistical units in the input
#' object with the values of the new variable for each of these units or
#' alternatively an object of class \code{\link{expression}} with a mathematical
#'  formula for the computation of these values.
#'
#' @param by Character vector with the names of the variables specifying those
#' statistical units groups under which the mathematical formula will be
#' applied.
#'
#' @param lag Integer vector of length 1 with the lag to use when the input
#' object is of class \linkS4class{StQList}.
#'
#' @return Object of the same class as the input object with the new variable
#' included.
#'
#' @import data.table
#'
#' @include StQ-class.R dcast_StQ.R getData.R getDD.R setData.R getUnits.R plus.StQ.R
#'
#' @examples
#' library(data.table)
#' newVNCVar <- new(Class = 'VNCdt',
#'                  data.table(IDQual = '', NonIDQual = '', IDDD = 'lTurnover',
#'                             Unit1 = ''))
#' newVNC <- new(Class = 'VarNameCorresp', list(Aggregates = newVNCVar))
#' newDD <- new(Class = 'DD',
#'              VarNameCorresp = newVNC, 
#'              Aggregates = new(Class = 'DDdt',
#'                              data.table(Variable = 'lTurnover',
#'                                         Sort = 'IDDD',
#'                                         Class = 'numeric',
#'                                         Qual1 = 'NOrden',
#'                                         ValueRegExp = '')))
#' NewQ <- setVar(object = Q,
#'                newDD = newDD, 
#'                Value = expression(log(1 + Turnover)))
#' getVar(NewQ, 'lTurnover')
#'
#' @export
setGeneric("setVar", function(object,
                              newDD,
                              Value,
                              lag = NULL,
                              by = NULL) {standardGeneric("setVar")})

#' @rdname setVar
#'
#' @include StQ-class.R DD-class.R getData.R getDD.R getUnits.R setDD.R setData.R dcast_StQ.R plus.StQ.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "setVar",
    signature = c("StQ", "DD"),
    function(object,
             newDD,
             Value,
             lag = NULL,
             by = NULL){

        if (missing(newDD)) {

            stop("[StQ::setVar] A new DD object for the new variable is needed.")
        }
        if (length(getVNC(newDD)) != 1 | dim(getVNC(newDD)[[1]])[1] != 1) {

            stop('[StQ::setVar] Only one new variable at a time.')
        }

        if (missing(Value)) {

            stop('[StQ::setVar] Value must be a vector of values or an object of class expression.')
        }

        if (!class(Value) %in% c('expression', 'integer', 'numeric', 
                                 'character', 'logical')) {
            stop('[StQ::setVar] Value must be an atomic vector or of class expression.')
        }

        NewData <- getUnits(object)
        
        pasteNA <- function(x, y){
            out <- ifelse(is.na(y) | y == '', paste0(x, ''), paste(x, y, sep ="_"))
            return(out)
        }
        
        newVNC <- copy(getVNC(newDD)[[1]])
        newVNC <- newVNC[, 'IDQual' := NULL, with = F]
        newVNC <- newVNC[, 'NonIDQual' := NULL, with = F]
        UnitCols <- names(newVNC)[grep('Unit', names(newVNC))]
        newVNC <- newVNC[, UnitCols := NULL, with = F]
        NewVarName <- newVNC[['IDDD']]
        for (col in names(newVNC)[-1]){
            
            NewVarName <- pasteNA(NewVarName, newVNC[[col]])
            
        }

        Data <- getData(object)
        if (NewVarName %in% Data[['IDDD']]) {

            setData(object) <- Data[IDDD != NewVarName]
        }
        
        DD <- getDD(object)
        newDD <- DD + newDD

        if (class(Value) == 'expression'){

            ExprVariables <- c(all.vars(Value), by)

            Data <- getData(object, ExprVariables[[1]])
            newObject <- new(Class = 'StQ', Data = Data, DD = newDD)

            Data <- dcast_StQ(newObject)

            if (is.null(by)){

                Data[, Value := eval(Value)]


            } else {

                setkeyv(Data, by)
                Data[, Value := eval(Value), by = eval(by)]
            }

            NewData <- Data[, ExprVariables := NULL, with = F]
            NewData[, IDDD := NewVarName]
            setcolorder(NewData,
                        c(setdiff(names(NewData), c('Value', 'IDDD')),
                          'IDDD', 'Value'))
            newObject <- new(Class = 'StQ', Data = NewData, DD = newDD)
            output <- object + newObject

        } else {

            NewData[, IDDD := NewVarName]
            NewData[, Value := Value]
            setkeyv(NewData, setdiff(names(NewData), 'Value'))
            NewObject <- new(Class = 'StQ', Data = NewData, DD = newDD)
            output <- object + NewObject

        }

        return(output)
    }
)
