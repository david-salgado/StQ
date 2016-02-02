#' @title Return a list of formulas for dcasting long data sets.
#'
#' @description \code{VarNamesToFormula} returns a list of formulas (in the
#' sense of functions \code{\link[data.table]{dcast}} and
#' \code{\link[data.table]{melt}} from the package \link{data.table}) for each
#' compound variable name included in the input vector \code{VarNames}.
#'
#' This function returns a \linkS4class{data.table} with two colums: the column
#' \code{Variable} with the root names of each compound variable name, and the
#' column \code{Form} with the corresponding formula having IDQual qualifiers
#' and the sort of variable (\code{IDDD}, \code{IDQual} or \code{NonIDQual}) in
#' its lhs and the variable qualifiers whose values appear as suffixes in the
#' compound variable names in the rhs. Duplicate entries in the resulting
#' \linkS4class{data.table} drop out.
#'
#' \code{VarNamesToFormula} is designed to be applied on compound variable names
#' including suffixes with qualifier values. It is fundamentally designed for
#' the internal use in the construction of editing strategies, but it can also
#' be of utility in some scripts.
#'
#' @param VarNames Character vector with compound variable names.
#'
#' @param DD Object of class \linkS4class{DD} with the definition and properties
#' of the variables.
#'
#' @return \linkS4class{data.table} with columns \code{Variable} and
#' \code{Form}. The column \code{Variable} indicates the root name of each input
#' variable name and the column \code{Form} presents the corresponding formula.
#'
#' @examples
#' # We build a DD object to be used as the second input parameter:
#' data(ExampleDD)
#' VarNamesToFormula(c('IASSEmpleo_1_1', 'IASSEmpleo_0', 'IASSEmpleo'), ExampleDD)
#'
#' @export
setGeneric("VarNamesToFormula",
           function(VarNames, DD){standardGeneric("VarNamesToFormula")})
#' @rdname VarNamesToFormula
#'
#' @include DD-class.R ExtractNames.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "VarNamesToFormula",
    signature = c("character", "DD"),
    function(VarNames, DD){

        trim <- function (x) gsub("^\\s+|\\s+$", "", x, useBytes = T)

        # Para una variable
        if (is.character(VarNames) & length(VarNames) == 1){
            
            DDSlotNames <- setdiff(slotNames(DD), 'VarNameCorresp')
            output <- list()
            for (DDslot in DDSlotNames){

                DDlocal <- slot(DD, DDslot)
                IDQual <- DDlocal[Sort == 'IDQual', Variable]
                NonIDQual <- DDlocal[Sort == 'NonIDQual', Variable]

                Quals <- names(DDlocal)[grep('Qual', names(DDlocal))]
                auxDD <- DDlocal[Variable == ExtractNames(VarNames),
                            c('Variable', Quals),
                            with = F]
                auxDD[, LHS := '']
                auxDD[, RHS := '']

                for (Qual in Quals){

                    auxDD[, LHS := ifelse(get(Qual) %in% IDQual,
                                          trim(paste(LHS, get(Qual))),
                                          trim(LHS))]
                    auxDD[, RHS := ifelse(get(Qual) %in% NonIDQual,
                                          trim(paste(RHS, get(Qual))),
                                          trim(RHS))]
                }

                auxDD[, Form := ifelse(LHS != '',
                                       ifelse(RHS != '',
                                              paste(gsub(' ', ' + ', trim(LHS)),
                                                    ' ~ IDDD +',
                                                    gsub(' ', ' + ', trim(RHS))),
                                              paste(gsub(' ', ' + ', trim(LHS)),
                                                    '~ IDDD')),
                                       paste0(gsub(' ', ' + ', trim(RHS)),
                                              ' ~ IDDD'))]
                auxDD <- auxDD[, c('Variable', 'Form'), with = F]
                auxDD[, Form := as.character(Form)]
                output[[DDslot]] <- auxDD
            }
            outputGlobal <- Reduce(function(x, y){
                merge(x, y, all = TRUE,
                      by = intersect(names(x), names(y)))},
                output)
            return(outputGlobal)


        } else {# Ahora para el resto de variables

            out.list <- lapply(as.list(VarNames), VarNamesToFormula, DD = DD)

            if (length(out.list) == 1){

                out <- out.list[[1L]]

            } else {

                out <- Reduce(
                        function(x, y){
                            merge(x, y, all = TRUE,
                                  by = intersect(names(x), names(y)))},
                        out.list)
            }
            return(out)
        }
    }
)
