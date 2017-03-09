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
#' library(data.table)
#' data(ExampleDD)
#' data(ExampleDatadt)
#' Q <- new(Class = 'StQ', Data = ExampleDatadt, DD = ExampleDD)
#' Q
#' # Notice that only the slot Data appears on screen, but the object is not a Datadt data.table:
#' str(Q)
#'
#' @include DD.R BuildDD.R
#'
#' @import data.table
#'
#' @export
StQ <- function(Data = data.table(IDDD = character(0), Value = character(0)),
                DD = BuildDD()){
            
    object <- list(Data = Data, DD = DD)
    Datadt <- object$Data
    DD <- object$DD
    
    NCol_Data <- dim(Datadt)[2]
    ColNames <- names(Datadt)
    if (ColNames[NCol_Data] != 'Value') {
        
        stop('[StQ:: validity StQ] The last column of component Data must be Value.')
        
    }
    if (ColNames[NCol_Data - 1] != 'IDDD') {
        
        stop('[StQ:: validity StQ] The last second column of compnent Data must be IDDD.')
    }

    # Si un identificador de unidad o variable está idénticamente en blanco, esta columna se elimina
    colData <- names(Datadt)
    colsData <- c('IDDD', 'Value')
         
    # Detección de filas duplicadas
    if (dim(Datadt)[[1]] != 0){
                 
        setkeyv(Datadt, colData[-which(colData == 'Value')])
        DupRows <- duplicated(Datadt, by = key(Datadt))
        if (sum(DupRows) > 0) {
            warning('[StQ::validity StQ] The following rows are duplicated:\n\n')
            print(Datadt[DupRows])
            stop('[StQ::validity StQ] Please remove duplicated rows.')
        }
    }
             
    # Comparamos los calificadores en los slots Data y DD: Todos los calificadores en Data deben estar definidos en algún slot de DD
    QualinData <- sort(setdiff(colData, colsData))
    QualinDD <- c()
    IDDDinDD <- c()
             
    DDslotNames <- setdiff(names(DD), 'VNC')
    for (DDslot in DDslotNames){
                 
        DDlocal <- DD[[DDslot]]
         QualinDD <- unique(c(QualinDD, DDlocal[Sort != 'IDDD'][['Variable']]))
        IDDDinDD <- unique(c(IDDDinDD, DDlocal[Sort == 'IDDD'][['Variable']]))
    }

    # Comparamos los calificadores en los slots Data y DD: Todos los calificadores en Data deben estar definidos en algún slot de DD
    if (length(QualinData) > 0 && !all(QualinData %in% QualinDD)) {
    
        stop(paste0('[StQ::validity StQ]  Columns not being "IDDD" and "Value" of slot Data must be specified as "IDQual" or "NonIDQual" in slot DD.'))
    
    }
             
    # Comparamos las variables en los slots Data y DD: Todas las variables en Data deben estar definidas en algún slot de DD
    IDDDinData <- unique(Data[['IDDD']])
    NotinDD <- setdiff(IDDDinData, c(IDDDinDD, ''))
    if (length(NotinDD) > 0) {
        
        stop(paste0('\n[StQ::validity StQ] The following variables in the column IDDD of slot "Data" are not defined in slot DD: \n',
                             paste0(NotinDD, collapse = ', '), '.\n'))
    }
             
    class(object) <- append("StQ", class(object))
    return(object)    
}

setOldClass(c('list'))
setOldClass(c('StQ', 'list'))


