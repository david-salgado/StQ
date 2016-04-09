#' @title S4 class for sets of \emph{St}andarized \emph{Q}uestionnaires
#'
#' @description Definition of an S4 class named \code{StQ} for sets of
#' standardized questionnaires.
#'
#' The structure of the class \code{StQ} comprises 2 attribute:
#' \itemize{
#' \item The attribute \code{Data}, which is a \linkS4class{data.table} with
#' key-value pair structure containing all statistical variables,both from the
#' questionnaire and any resulting metadata from the data processing.
#'
#' \item The attribute \code{DD}, which is an object of class \linkS4class{DD}.
#' It basically contains the definition and properties of each variable.
#' }
#'
#' Every variable name in the attribute \code{Data} must be present in the
#' attribute \code{DD}.
#'
#' @slot Data Object of class \linkS4class{data.table} with key-value pair
#' structure. It must have at least two columns: \code{IDDD} and \code{Value}.
#' It contains all statistical variables (including some metadata) together with
#' their corresponding values. If \code{Data} is not specified as an input
#' parameter, an empty \linkS4class{data.table} with columns \code{IDDD} and
#' \code{Value} will be initiated.
#'
#' @slot DD Object of class \linkS4class{DD} with the definition and properties
#' of all variables. If \code{DD} is not specified as an input parameter, an
#' empty \linkS4class{DD} object with columns \code{Variable}, \code{Sort},
#' \code{Class} and \code{Qual1} will be initiated.
#'
#' @examples
#' # An empty standardized questionnaire set:
#' new(Class = 'StQ')
#'
#' # An example with data created previosly:
#' library(data.table)
#' data(DataTot)
#' data(ExampleDD)
#' Q <- new(Class = 'StQ', Data = DataTot, DD = ExampleDD)
#' 
#' # An empty data set:
#' library(data.table)
#' Data <- data.table(IDDD = character(0), Value = character(0))
#'
#' # An object DD:
#' DDData <- data.table(Variable = c('NOrden', 'EsRemuner', 'Mes', 'Anno',
#'                                   'CCAA','CNAE2009', 'CifraNeg', 'Empleo'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD', 'IDDD',
#'                               'IDDD', 'IDDD', 'IDDD', 'IDDD'),
#'                      Class = c('character', 'integer', 'character', 'character',
#'                                'character', 'character', 'numeric', 'integer'),
#'                      Qual1 = c('', '', 'NOrden', 'NOrden',
#'                                'NOrden', 'NOrden', 'NOrden', 'NOrden'),
#'                      Qual2 = c('', '', '', '',
#'                                '', '', '', 'EsRemuner'))
#' VNCList <- list(MicroData= 
#'                  data.table(IDQual = c('NOrden', '', '', '', '', '', '', ''),
#'                             NonIDQual = c('', 'EsRemuner', '', '', '', '', '',
#'                                           ''), 
#'                             IDDD = c('', '', 'Mes', 'Anno', 'CCAA',
#'                                      'CNAE2009', 'CifraNeg', 'Empleo'), 
#'                             NOrden = c('', '', '', '', '', '', '', ''), 
#'                             EsRemuner = c('', '', '', '', '', '', '', ''), 
#'                             Unit1 = c('', '', '', '', '', '', '', '')))
#' VNC <- new(Class = 'VarNameCorresp', VarNameCorresp = VNCList)
#' DD <- new(Class = 'DD', MicroData = DDData, VarNameCorresp = VNC)
#'
#' # We create an object StQ with no data
#' Q <- new(Class = 'StQ', Data = Data, DD = DD)
#' Q
#' # Notice that only the slot Data appears on screen, but the object is not a
#' # data.table:
#' str(Q)
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setClass(Class = "StQ",
         slots = c(Data = 'data.table',
                   DD = 'DD'),
         prototype = list(Data = data.table(IDDD=character(0),
                                            Value=character(0)),
                          DD = new(Class = 'DD')),
         validity = function(object){

             Data <- object@Data
             colData <- names(Data)
             # Data debe tener al menos dos columnas: IDDD y Value
             if (colData[length(colData) - 1] != 'IDDD'){
                 stop(paste0('[Validity StQ] The last but one of the columns of slot "Data" must have name "IDDD".'))
             }
             if (colData[length(colData)] != 'Value') {
                 stop(paste0('[Validity StQ] The last column of slot "Data" must have name "Value".'))
             }
             
             # Si un identificador de variable está idénticamente en blanco, esta columna se elimina
             colsData <- c('IDDD', 'Value')
             for (col in setdiff(colData, colsData)){
                 
                 if (all(Data[[col]] == '')) Data[, col := NULL, with = F]
             }
             
             object@Data <- Data
             colData <- names(Data)
            
             # Detección de filas duplicadas
             if (dim(Data)[[1]] != 0){
                 
                 setkeyv(Data, colData[-which(colData == 'Value')])
                 DupRows <- duplicated(Data)
                 if (sum(DupRows) > 0) {
                     warning('[Validity StQ] The following rows are duplicated:\n\n')
                     print(Data[DupRows])
                     stop('[Validity StQ] Please remove duplicated rows.')
                 }
             }
             
             # Las columnas IDQual y NonIDQual deben tener la clase especificada en el slot DD
             QualinData <- sort(setdiff(colData, c('IDDD', 'Value')))
             QualClassinData <- sapply(Data, class)[QualinData]
             QualClassinData <- sort(QualClassinData)
             
             
             # Recorremos el slot DD para identificar los distintos calificadores
             DDslotNames <- setdiff(slotNames(object@DD), 'VarNameCorresp')
             QualinDD <- c()
             IDDDinDD <- c()
             for (DDslot in DDslotNames){

                 DDlocal <- slot(object@DD, DDslot)
                 
                 IDDDinDD <- unique(c(IDDDinDD, DDlocal[Sort == 'IDDD'][['Variable']]))
                 QualClassinDD <- DDlocal[Sort != 'IDDD'][['Class']]
                 QualinDD <- DDlocal[Sort != 'IDDD'][['Variable']]
                 names(QualClassinDD) <- QualinDD
                 QualClassinDD <- sort(QualClassinDD[QualinData])
                 if (length(QualClassinDD) > 0 &&
                     !all(sort(QualClassinData[names(QualClassinDD)]) == QualClassinDD)) {
                     stop(paste0('[Validity StQ] The class of at least one qualifier in the slot Data does not coincide with that of ', DDslot, ' in slot DD.'))
                 }
             }
             
             # Comparamos los calificadores en los slots Data y DD: Todos los calificadores en Data deben estar definidos en algún slot de DD
             
             if (length(QualinDD) > 0 && !all(QualinData %in% QualinDD)) {
                 stop(paste0('[Validity StQ] Columns not being "IDDD" and "Value" of slot Data must be specified as "IDQual" or "NonIDQual" in slot DD.'))
             }
             
             # Comparamos las variables en los slots Data y DD. Todas las variables en Data deben estar definidas en algún slot de DD
             IDDDinData <- unique(Data[['IDDD']])
             NotinDD <- setdiff(IDDDinData, IDDDinDD)
             if (length(NotinDD) > 0) {
                     stop(paste0('\n[Validity StQ] The following variables in the column IDDD of slot "Data" are not defined in slot DD: \n',
                                 paste0(NotinDD, collapse = ', '), '.\n'))
             }
             
             
             
             return(TRUE)
         }
)
