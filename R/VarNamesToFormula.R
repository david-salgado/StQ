#' @title Return a list of formulas for dcasting long data sets.
#'
#' @description \code{VarNamesToFormula} returns a list of formulas (in the sense of functions
#' \code{\link[data.table]{dcast}} and \code{\link[data.table]{melt}} from the package
#' \link{data.table}) for each compound variable name included in the input vector \code{VarNames}.
#'
#' This function returns a \linkS4class{data.table} with two colums: the column \code{Variable} with
#' the root names of each compound variable name, and the column \code{Form} with the corresponding
#' formula having IDQual qualifiers and the sort of variable (\code{IDDD}, \code{IDQual} or
#' \code{NonIDQual}) in its lhs and the variable qualifiers whose values appear as suffixes in the
#' compound variable names in the rhs. Duplicate entries in the resulting \linkS4class{data.table}
#' drop out.
#'
#' \code{VarNamesToFormula} is designed to be applied on compound variable names including suffixes
#' with qualifier values. It is fundamentally designed for the internal use in the construction of
#' editing strategies, but it can also be of utility in some scripts.
#'
#' @param VarNames Character vector with compound variable names.
#'
#' @param DD Object of class \link{DD} with the definition and properties of the variables.
#'
#' @return \linkS4class{data.table} with columns \code{Variable} and \code{Form}. The column
#' \code{Variable} indicates the root name of each input variable name and the column \code{Form}
#' presents the corresponding formula.
#'
#' @examples
#' data(ExampleDD)
#' VarNamesToFormula('Turnover', ExampleDD)
#' 
#' VarNamesToFormula(c('Employees_1.', 'Employees_2.2', 'Employees'), ExampleDD)
#' 
#' VarNamesToFormula(c('Turnover', 'Employees_1.', 'Employees_2.2', 'Employees'), ExampleDD)
#'
#' @export
setGeneric("VarNamesToFormula",
           function(VarNames, DD){standardGeneric("VarNamesToFormula")})
#' @rdname VarNamesToFormula
#'
#' @include VNC.R DD.R ExtractNames.R getVariables.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "VarNamesToFormula",
    signature = c("character", "DD"),
    function(VarNames, DD){
        
        trimPlus <- function (x) gsub("^\\s{1}[+]{1}\\s{1}|\\s{1}[+]{1}\\s{1}$", "", x, useBytes = T)
        #trim <- function (x) gsub("^\\s+|\\s+$", "", x, useBytes = T)
        
        NotPresentVar  <- setdiff(ExtractNames(VarNames), getVariables(DD))
        if (length(NotPresentVar) > 0) stop(paste0('[StQ::VarNamesToDD] The following variables are not contained in the DD slot: ', NotPresentVar, '.\n'))
        IDQuals <- getIDQual(DD)
        DotQuals <- setdiff(IDQuals, getDotQual(DD))
        DDdt <- rbindlist(lapply(DD, function(DT){DT})[-1], fill = TRUE)[Sort == 'IDDD']
        AllCols <- c('Variable', names(DDdt)[grep('Qual', names(DDdt))])
        ConcatCols <- setdiff(AllCols, 'Variable') 
        setkeyv(DDdt, AllCols)
        DDdtUnique <- DDdt[!duplicated(DDdt, by = key(DDdt)), AllCols, with=FALSE][
            Variable %chin% ExtractNames(VarNames)]
        IDQuals <- getIDQual(DD)
        dotQuals <- setdiff(getDotQual(DD), IDQuals)
        trimPlus <- function (x) gsub("^\\s{1}[+]{1}\\s{1}|\\s{1}[+]{1}\\s{1}$", "", x, useBytes = T)
        for (i in seq_along(ConcatCols)){
            
            qual <- paste0('Qual', i)
            if (i == 1) { 
                
                DDdtUnique[, LHS := ifelse(get(qual) %chin% c(IDQuals, dotQuals), get(qual), '')]
                DDdtUnique[, RHS := ifelse(!get(qual) %chin% c(IDQuals, dotQuals), paste('IDDD', get(qual), sep = ' + '), 'IDDD')]
                
            } else {
                
                DDdtUnique[, LHS := ifelse(get(qual) %chin% c(IDQuals, dotQuals), paste(trimPlus(LHS), get(qual), sep = ' + '), LHS)]
                DDdtUnique[, RHS := ifelse(!get(qual) %chin% c(IDQuals, dotQuals), paste(trimPlus(RHS), get(qual), sep = ' + '), RHS)]
                
            }
        }
        DDdtUnique[, RHS := trimPlus(RHS)]
        DDdtUnique <- DDdtUnique[, Form := paste(LHS, RHS, sep = ' ~ ')][, c('Variable', 'Form'), with = FALSE]
        
        return(DDdtUnique)
    }
)
