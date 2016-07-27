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
#' structure. It must have exactly three columns: \code{IDDDKey},  \code{QualKey} and \code{Value}.
#' It contains all statistical variables (including some metadata) together with
#' their corresponding values. If \code{Data} is not specified as an input
#' parameter, an empty \linkS4class{rawDatadt} object with columns \code{IDDDKey}, \code{QualKey} 
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
<<<<<<< HEAD
#' key <- new(Class = 'rawKey', data.table(IDDDKey = c('Turnover', 'Employees', 'Employees', 
#'                                                     'Employees'),
#'                                         QualKey = c('25648544SS001  ', 
#'                                                     '25648544SS00110', 
#'                                                     '25648544SS0010 ', 
#'                                                     '25648544SS00111')))
#' rawData <- new(Class = 'rawDatadt', 
#'                data.table(Key = key, Value = c('2034120', '414', '0')))
||||||| merged common ancestors
#' key <- new(Class = 'rawKey', 
#'            c('Turnover@@001@@@@', 
#'              'Employees@@001@@1@@0', 
#'              'Employees@@001@@0@@', 
#'              'Employees@@001@@1@@1'))
#' rawData <- new(Class = 'rawDatadt', 
#'                data.table(Key = key, Value = c('2034120', '414', '0')))
=======
#' key <- new(Class = 'rawKey', 
#'           data.table(IDDDKey = c('Employees', 'Employees', 'RemEmployees', 'Turnover'),
#'                      QualKey = c('25641378SS2.1.1.', '25641378SS1.    ', '25641378SS    1.', 
#'                                  '25641378SS')))
#' value = c('625', '954', '122', '105124')
#' rawData <- BuildrawDatadt(key, value)
>>>>>>> dd5b9b2bd7b111f94c123cf9619690800f3602b6
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
                 
                 setkeyv(Data, c('IDDDKey', 'QualKey'))
                 DupRows <- duplicated(Data)
                 if (sum(DupRows) > 0) {
                     warning('[Validity rawStQ] The following rows are duplicated:\n\n')
                     print(Data[DupRows])
                     stop('[Validity rawStQ] Please remove duplicated rows.')
                 }
             }
             
             ## LAS SIGUIENTES COMPROBACIONES LAS DEJAMOS PARA StQ PORQUE ES PRECISO
             ## PARSEAR LAS CLAVES PARA HACERLAS, LO QUE REQUIERE CONSUMO DE RECURSOS
             
             # Comparamos los calificadores en los slots Data y DD: Todos los calificadores en Data deben estar definidos en algún slot de DD
             #QualinDD <- c()
             #IDDDinDD <- c()
             
             #DDslotNames <- setdiff(slotNames(object@DD), 'VarNameCorresp')
             #for (DDslot in DDslotNames){

             #   DDlocal <- slot(object@DD, DDslot)
             #    QualinDD <- unique(c(QualinDD, DDlocal[Sort != 'IDDD'][['Variable']]))
             #    IDDDinDD <- unique(c(IDDDinDD, DDlocal[Sort == 'IDDD'][['Variable']]))
             #}
            
             #QualinDD <- length(QualinDD)
             #QualinData <- lapply(strsplit(Data@.Data[[1]], '@@'), length)
             #QualinData <- Reduce(max, QualinData) - 1
             #if (QualinData > QualinDD){
                 
             #    stop('[Validity rawStQ] Length in some element in column key of slot Data is not correct.')
             #}

             # Comparamos las variables en los slots Data y DD: Todas las variables en Data deben estar definidas en algún slot de DD
             #IDDDinData <- unlist(lapply(strsplit(Data@.Data[[1]], '@@'), function(x){x[1]}))
             #NotinDD <- setdiff(IDDDinData, IDDDinDD)
             #if (length(NotinDD) > 0) {
             #    stop(paste0('\n[Validity rawStQ] The following variables included in column Key of slot Data are not defined in slot DD: \n',
             #                paste0(NotinDD, collapse = ', '), '.\n'))
             #}

             return(TRUE)
         }
)
