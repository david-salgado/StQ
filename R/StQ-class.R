#' @title S4 class for sets of \emph{St}andarized \emph{Q}uestionnaires
#'
#' @description Definition of an S4 class named \code{StQ} for sets of standardized questionnaires.
#'
#' The structure of the class \code{StQ} comprises 2 attributes:
#' \itemize{
#' \item The attribute \code{Data}, which is an object of class \linkS4class{Datadt}, that is, a 
#' \linkS4class{data.table} with at least the columns \code{IDDD} and \code{Value}.
#'
#' \item The attribute \code{DD}, which is an object of class \linkS4class{DD}. It basically 
#' contains the definition and properties of each variable.
#' }
#'
#' Every variable name in the attribute \code{Data} must be present in the attribute \code{DD}.
#'
#' @slot Data Object of class \linkS4class{Datadt}. It must have at least two columns: \code{IDDD} 
#' and \code{Value}. It contains all statistical variables (including some metadata) together with 
#' their corresponding values. If \code{Data} is not specified as an input parameter, an empty
#' \linkS4class{Datadt} object with columns \code{IDDD} and \code{Value} will be initiated.
#'
#' @slot DD Object of class \linkS4class{DD} with the definition and properties of all variables. If
#'  \code{DD} is not specified as an input parameter, an empty \linkS4class{DD} object with columns 
#'  \code{Variable}, \code{Sort}, \code{Class}, \code{Qual1} and \code{ValueRegExp} will be 
#'  initiated.
#' 
#' @examples
#' # An empty standardized questionnaire set:
#' new(Class = 'StQ')
#' 
#' 
#' library(data.table)
#' data(ExampleDD)
#' data(ExampleDatadt)
#' Q <- new(Class = 'StQ', Data = ExampleDatadt, DD = ExampleDD)
#' Q
#' # Notice that only the slot Data appears on screen, but the object is not a Datadt data.table:
#' str(Q)
#'
#' @include DD-class.R Datadt-class.R DatadtToDT.R
#'
#' @import data.table
#'
#' @export
setClass(Class = "StQ",
         slots = c(Data = 'Datadt',
                   DD = 'DD'),
         prototype = prototype(list(Data = new(Class = 'Datadt'),
                          DD = new(Class = 'DD'))),
         validity = function(object){

             Datadt <- object@Data
             
             # Si un identificador de unidad o variable está idénticamente en blanco, esta columna se elimina
             colData <- names(Datadt)
             colsData <- c('IDDD', 'Value')
             Data <- DatadtToDT(Datadt)
             for (col in setdiff(colData, colsData)){
                 
                 if (all(Data[[col]] == '')) Data[, (col) := NULL]
             }
             
             object@Data <- new(Class = 'Datadt', Data)
             colData <- names(Data)
            
             
             # Detección de filas duplicadas
             if (dim(Data)[[1]] != 0){
                 
                 setkeyv(Data, colData[-which(colData == 'Value')])
                 DupRows <- duplicated(Data, by = key(Data))
                 if (sum(DupRows) > 0) {
                     warning('[StQ::validity StQ] The following rows are duplicated:\n\n')
                     print(Data[DupRows])
                     stop('[StQ::validity StQ] Please remove duplicated rows.')
                 }
             }
             

             # Comparamos los calificadores en los slots Data y DD: Todos los calificadores en Data deben estar definidos en algún slot de DD
             QualinData <- sort(setdiff(colData, colsData))
             QualinDD <- c()
             IDDDinDD <- c()
             
             DDslotNames <- setdiff(slotNames(object@DD), 'VarNameCorresp')
             for (DDslot in DDslotNames){
                 
                 DDlocal <- DatadtToDT(slot(object@DD, DDslot))
                 QualinDD <- unique(c(QualinDD, DDlocal[Sort != 'IDDD'][['Variable']]))
                 IDDDinDD <- unique(c(IDDDinDD, DDlocal[Sort == 'IDDD'][['Variable']]))
             }

             # Comparamos los calificadores en los slots Data y DD: Todos los calificadores en Data deben estar definidos en algún slot de DD
             if (length(QualinData) > 0 && !all(QualinData %in% QualinDD)) {
                 stop(paste0('[StQ::validity StQ]  Columns not being "IDDD" and "Value" of slot Data must be specified as "IDQual" or "NonIDQual" in slot DD.'))
             }
             
             # Comparamos las variables en los slots Data y DD: Todas las variables en Data deben estar definidas en algún slot de DD
             IDDDinData <- unique(Data[['IDDD']])
             NotinDD <- setdiff(IDDDinData, IDDDinDD)
             if (length(NotinDD) > 0) {
                 stop(paste0('\n[StQ::validity StQ] The following variables in the column IDDD of slot "Data" are not defined in slot DD: \n',
                             paste0(NotinDD, collapse = ', '), '.\n'))
             }
             
             return(TRUE)
         }
)
