#' @title S4 class for sets of \emph{raw} \emph{St}andarized 
#' \emph{Q}uestionnaires
#'
#' @description Definition of an S4 class named \code{rawStQ} for sets of raw
#' standardized questionnaires.
#'
#' The structure of the class \code{rawStQ} comprises 2 attribute:
#' \itemize{
#' \item The attribute \code{Data}, which is an object of class 
#' \linkS4class{rawDatadt} with key-value pair structure containing all
#' statistical variables, both from the questionnaire and any resulting metadata
#' from the data processing.
#'
#' \item The attribute \code{DD}, which is an object of class \linkS4class{DD}.
#' It basically contains the definition and properties of each variable.
#' }
#'
#' @slot Data Object of class \linkS4class{rawDatadt} with key-value pair
#' structure. It must have exactly two columns: \code{Key} and \code{Value}.
#' It contains all statistical variables (including some metadata) together with
#' their corresponding values. If \code{Data} is not specified as an input
#' parameter, an empty \linkS4class{rawDatadt} object with columns \code{Key} 
#' and \code{Value} will be initiated.
#'
#' @slot DD Object of class \linkS4class{DD} with the definition and properties
#' of all variables. If \code{DD} is not specified as an input parameter, an
#' empty \linkS4class{DD} object with columns \code{Variable}, \code{Sort},
#' \code{Class}, \code{Qual1} and \ code{ValueRegExp} will be initiated.
#'
#' @examples
#' # An empty standardized questionnaire set:
#' new(Class = 'rawStQ')
#'
#'
#' library(data.table)
#' data(ExampleDD)
#' key <- new(Class = 'rawKey', key = c('NOrden', 'CCAA', 'EsRemuner', 'TipoRem', 'RamaCNAE09', 
#'                               'IDRefPond1', 'VarPonder', 'DivisionCNAE09', 'IDRefPond2', 
#'                               'SectorCNAE09', 'GeneralOtrosCNAE09', 'IDRefPond3', 'IDDD'),   
#'                       Data = c('IASS@@9644947400S@@@@@@@@@@@@@@@@@@@@@@@@IASSCifraNeg',
#'                                'IASS@@9644947400S@@@@1@@1@@@@@@@@@@@@@@@@@@IASSEmpleo',
#'                                'IASS@@9644947400S@@03@@@@@@@@@@@@@@@@@@@@@@IASSLPCifraNeg'))
#' rawData <- new(Class = 'rawDatadt', 
#'                data.table(Key = key, Value = c('2034120', '414', '0')))
#' 
#' rawQ <- new(Class = 'rawStQ', Data = rawData, DD = ExampleDD)
#' 
#' @include DD-class.R rawDatadt-class.R
#'
#' @import data.table
#'
#' @export
setClass(Class = "rawStQ",
         slots = c(Data = 'rawDatadt',
                   DD = 'DD'),
         prototype = list(Data = new(Class = 'rawDatadt'),
                          DD = new(Class = 'DD')),
         validity = function(object){

             Data <- getData(object)

             # Detección de filas duplicadas
             if (dim(Data)[[1]] != 0) {
                 
                 setkeyv(Data, 'Key')
                 DupRows <- duplicated(Data)
                 if (sum(DupRows) > 0) {
                     warning('[Validity rawStQ] The following rows are duplicated:\n\n')
                     print(Data[DupRows])
                     stop('[Validity rawStQ] Please remove duplicated rows.')
                 }
             }
             
             
             # # Comparamos los calificadores en los slots Data y DD: Todos los calificadores en Data deben estar definidos en algún slot de DD
             # QualinData <- sort(setdiff(names(KeyToDT(Data[['Key']])), 'IDDD'))
             QualinDD <- c()
             IDDDinDD <- c()

             DDslotNames <- setdiff(slotNames(object@DD), 'VarNameCorresp')
             for (DDslot in DDslotNames) {

                 DDlocal <- slot(object@DD, DDslot)
                 # DDlocal <- slot(DD, DDslot)
                 QualinDD <- unique(c(QualinDD, DDlocal[Sort != 'IDDD'][['Variable']]))
                 IDDDinDD <- unique(c(IDDDinDD, DDlocal[Sort == 'IDDD'][['Variable']]))
             }
             
             # # Comparamos los calificadores en los slots Data y DD: Todos los calificadores en Data deben estar definidos en algún slot de DD
             # if (length(QualinData) > 0 && !all(QualinData %in% QualinDD)) {
             #     stop(paste0('[Validity rawStQ] Variables included in column Key of slot Data not being "IDDD"  must be specified as "IDQual" or "NonIDQual" in slot DD.'))
             # }
             
             # Comparamos las variables en los slots Data y DD: Todas las variables en Data deben estar definidas en algún slot de DD
             # IDDDinData <- unique(KeyToDT(Data[['Key']])[['IDDD']])
             IDDDinData <- unique(KeyToDT(Data[['Key']])[[dim(KeyToDT(Data[['Key']]))[[2]]]])
             NotinDD <- setdiff(IDDDinData, IDDDinDD)
             if (length(NotinDD) > 0) {
                 stop(paste0('\n[Validity rawStQ] The following variables included in column Key of slot Data as "IDDD" are not defined in slot DD: \n',
                                 paste0(NotinDD, collapse = ', '), '.\n'))
             }
             
             return(TRUE)
         }
)
