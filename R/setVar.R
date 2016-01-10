#' @title Method to set values of a variable
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
#' specified as a data.table with a single row and columns Variable, Sort, Class
#' and Qual1-Qualn (as many as so many qualifiers are necessary).
#'
#' Input parameters \code{by} and \code{lag} are optional and specify how to
#' compute the values of the new variable in some circunstamces.
#'
#' @param object Object in which to include the new variable.
#'
#' @param VarName Character vector of length 1 with the name of the new
#' variable.
#'
#' @param Value Vector of length the number of statistical units in the input
#' object with the values of the new variable for each of these units or
#' alternatively an object of class \code{\link{expression}} with a mathematical
#'  formula for the computation of these values.
#'
#' @param DDnl \linkS4class{DD} object with a row of columns \code{Variable},
#'  \code{Sort}, \code{Class} and \code{Qual1} to \code{Qualn} with the
#'  information of the new variable needed for the slot \code{DD} of the input
#'  object.
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
#' data(ExampleQ)
#' newDD <- new(Class = 'DD', Data = data.table(Variable = 'lCN',
#'                                              Sort = 'IDDD',
#'                                              Class = 'numeric',
#'                                              Qual1 = 'NOrden'))
#' NewQ <- setVar(object = ExampleQ, VarName = 'lCN',
#'                Value = expression(log(1 + IASSCifraNeg)), DDnl = newDD)
#' getVar(NewQ, 'lCN')
#'
#' @export
setGeneric("setVar", function(object,
                              VarName,
                              Value,
                              DDnl,
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
    signature = c("StQ", "character", "ANY", "DD"),
    function(object,
             VarName,
             Value,
             DDnl,
             lag = NULL,
             by = NULL){

        if (missing(VarName)) {

            stop("[StQ::setVar] A name for the new variable is needed.")
        }
        if (length(VarName) != 1) {

            stop('[StQ::setVar] Only one new variable at a time.')
        }

        if (missing(Value)) {

            stop('[StQ::setVar] Value must be a vector of values or an object expression.')
        }
        if (!VarName %in% getData(getDD(object))[['Variable']] && missing(DDnl)){

            stop('[StQ::setVar] It is necessary to specify a new line for the slot DD corresponding to the new varible.')

        }
        if (!class(Value) %in% c('expression', 'integer', 'numeric',
                                 'character', 'logical')) {
            stop('[StQ::setVar] Value must be an atomic vector or of class expression.')
        }

        NewData <- getUnits(object)
        Data <- getData(object)
        DD <- getDD(object)

        if (VarName %in% Data[['IDDD']]) {

            setData(object) <- Data[IDDD != VarName]

        }

        newDD <- DD + DDnl

        if (class(Value) == 'expression'){

            ExprVariables <- c(all.vars(Value), by)

            Data <- getData(object, ExprVariables)
            newObject <- new(Class = 'StQ', Data = Data, DD = newDD)
            Data <- dcast_StQ(newObject)

            if (is.null(by)){

                Data[, Value := eval(Value)]


            } else {

                setkeyv(Data, by)
                Data[, Value := eval(Value), by = eval(by)]
            }

            NewData <- Data[, ExprVariables := NULL, with = F]
            NewData[, IDDD := VarName]
            setcolorder(NewData,
                        c(setdiff(names(NewData), c('Value', 'IDDD')),
                          'IDDD', 'Value'))
            newObject <- new(Class = 'StQ', Data = NewData, DD = newDD)
            output <- object + newObject

        } else {

            NewData[, IDDD := VarName]
            NewData[, Value := Value]
            setkeyv(NewData, setdiff(names(NewData), 'Value'))
            NewObject <- new(Class = 'StQ', Data = NewData, DD = newDD)
            output <- object + NewObject

        }

        return(output)
    }
)
