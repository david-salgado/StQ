#' @title S4 class for sets of \emph{raw} \emph{St}andarized \emph{Q}uestionnaires
#'
#' @description Definition of an S4 class named \code{rawStQ} for sets of raw standardized 
#' questionnaires.
#'
#' The structure of the class \code{rawStQ} comprises 2 attributes:
#' \itemize{
#' \item The attribute \code{rawData}, which is an object of class \linkS4class{data.table} with 
#' key-value pair structure containing all statistical variables, both from the questionnaire and 
#' any resulting metadata from the data processing.
#'
#' \item The attribute \code{DD}, which is an object of class \link{DD}. It basically 
#' contains the definition and properties of each variable.
#' }
#'
#' @slot rawData Object of class \linkS4class{data.table} with key-value pair structure. It must 
#' have exactly three columns: \code{IDDDKey},  \code{QualKey} and \code{Value}. It contains all 
#' statistical variables (including some metadata) together with their corresponding values. If 
#' \code{rawData} is not specified as an input parameter, an empty \linkS4class{data.table} object 
#' with columns \code{IDDDKey}, \code{QualKey} and \code{Value} will be initiated.
#'
#' @slot DD Object of class \link{DD} with the definition and properties of all variables. If
#'  \code{DD} is not specified as an input parameter, an empty \link{DD} object with all its
#'  components  will be initiated.
#'
#' @examples
#' # An empty standardized questionnaire set:
#' rawStQ()
#'
#' library(data.table)
#' data(ExampleDD)
#' rawData <- data.table(IDDDKey = c('Employees', 'Employees', 'RemEmployees', 'Turnover'),
#'                       QualKey = c('25641378SS2.1.1.', '25641378SS1.    ', '25641378SS    1.', '25641378SS'),
#'                       Value = c('625', '954', '122', '105124'))
#' rawStQ(rawData, ExampleDD)
#' 
#' @include DD.R
#'
#' @import data.table
#'
#' @export
rawStQ <- function(rawData = data.table(IDDDKey = character(0),
                                        QualKey = character(0),
                                        Value = character(0)),
                   DD = BuildDD()){
        
    
    object <- list(rawData = rawData, DD = DD)
    rawDatadt <- object$rawData
    DD <- object$DD

    ColNames_Data <- names(rawDatadt)
    if (ColNames_Data[1] != 'IDDDKey') {
        
        stop('[StQ::validity rawStQ] The first column of component rawData must be IDDDKey.')
    }
    
    if (ColNames_Data[2] != 'QualKey') {
        
        stop('[StQ::validity rawStQ] The second column of component rawData must be QualKey.')
    }
    
    if (ColNames_Data[3] != 'Value') {
        
        stop('[StQ::validity rawStQ] The third column of component rawData must be Value.')
    }
    
    Ncol <- length(ColNames_Data)
    if (Ncol != 3) {
        
        stop('[StQ::validity rawStQ] Only three columns are allowed in the component rawData.')    
        
    }
    
    # Detección de filas duplicadas
    if (dim(rawDatadt)[[1]] != 0) {
                 
        setkeyv(rawDatadt, c('IDDDKey', 'QualKey'))
        DupRows <- duplicated(rawDatadt, by = key(rawDatadt))
        if (sum(DupRows) > 0) {
            warning('[StQ::validity rawStQ] The following rows are duplicated:\n\n')
            print(rawDatadt[DupRows])
            stop('[StQ::validity rawStQ] Please remove duplicated rows.')
        }
    }
    class(object) <- append("rawStQ", class(object))
    return(object)    
}

setOldClass(c('list'))
setOldClass(c('rawStQ', 'list'))

