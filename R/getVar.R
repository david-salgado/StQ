#' @title Return the values of a variable of specified statistical units
#'
#' @description \code{getVar} returns the values of the variable specified as
#' input parameter of those statistical units also specified as an input
#' parameter of an input object.
#'
#' @param object Object of class \linkS4class{StQ}.
#'
#' @param Units \linkS4class{data.table} with the qualifier values identifying
#' each statistical unit in the input object.
#'
#' @param VarName Character vector with the name of the variable.
#'
#' @return Matrix with the queried values.
#'
#' @examples
#' library(data.table)
#' Units <- data.table(NOrden = 1:5)
#' getVar(ExampleQ, 'IASSCifraNeg', Units)
#' getVar(ExampleQ, 'IASSEmpleo', Units)
#' getVar(ExampleQ, 'IASSEmpleo_0', Units)
#' getVar(ExampleQ, 'IASSEmpleo_1', Units)
#' getVar(ExampleQ, 'IASSEmpleo_1_1', Units)
#'
#' @export
setGeneric("getVar", function(object, VarName, Units = getUnits(object)){standardGeneric("getVar")})

#' @rdname getVar
#'
#' @include StQ-class.R getData.R getDD.R VarNamesToDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getVar",
    signature = c("StQ", "character"),
    function(object, VarName, Units = getUnits(object)){

        if (length(VarName) != 1) {

            stop('[StQ::getVar] Only one variable can be specifed as input.')

        }
        Data <- getData(object)
        setkeyv(Data, names(Units))
        Data <- Data[Units]
        DD <- getDD(object)
        Var <- VarNamesToDD(VarName, DD)
        Data <- merge(Data, Var, by = names(Var))
        Data <- Data[, c(names(Units), 'Value'), with = F]
        if (dim(Data)[1] == 0) return(Data)
        Data[, Unit := '']
        for (col in setdiff(names(Data), c('Unit', 'Value'))){
            Data[, Unit := ifelse(Unit == '', get(col), paste0(Unit, '_', get(col)))]
        }
        output <- Data[['Value']]
        VarClass <- getData(DD)[Variable == ExtractNames(VarName)][['Class']]
        output <- as(output, VarClass)
        output <- matrix(output, ncol = 1, dimnames = list(Data[['Unit']], VarName))
        return(output)
    }
)
