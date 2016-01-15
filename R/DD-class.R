#' Clase S4 para la información contenida en los ficheros DD.
#'  
#' Definición de una clase S4 \code{DD} con la información contenida en los 
#' ficheros DD con un formato ligeramente distinto al del repositorio.
#' 
#' La estructura de la clase S4 \code{DD} se compone de un slot de clase 
#' \linkS4class{data.table} con, al menos, las columnas \code{Variable}, 
#' \code{Sort}, \code{Class} y \code{Qual1}. Estas columnas tienen los 
#' siguientes significados:
#' 
#' \itemize{
#'  \item \code{Variable}: Nombre de la variable.
#'  \item \code{Sort}: Tipo de la variable, que puede ser calificador de unidad
#'  (\code{IDQual}), calificador no de unidad (\code{NonIDQual}) y nombre de 
#'  variable (\code{IDDD}).
#'  \item \code{Class}: Clase de la variable (\code{integer}, \code{numeric}, 
#'  \code{character},...).
#'  \item \code{Qual1}: Nombre del calificador 1 de la variable. 
#' }
#' 
#' La \linkS4class{data.table} se completa con tantas columnas \code{Qualn} como
#' sean necesarias.
#' 
#' @slot DD \linkS4class{data.table} con, al menos, las columnas \code{Variable},
#'  \code{Sort}, \code{Class}, \code{Qual1} (en ese orden).
#'  
#' @examples
#' # Un objeto DD vacío se construye mediante el código:
#' new(Class = 'DD')
#' 
#' # Un ejemplo elemental con tres variables (1 calificador de unidad, 1 
#' # calificador de variable y 1 variable)
#' DDData <- data.table(Variable = c('NumIdEst', 'EsMercNac', 'EsMercEuro', 'EsMercRM', 'Cod', 'IEPEntradaPed'),
#'                      Sort = c('IDQual', 'NonIDQual', 'NonIDQual', 'NonIDQual', 'NonIDQual', 'IDDD'),
#'                      Class = c('character', 'character', 'character', 'character', 'character', 'character'),
#'                      Qual1 = c('', '', '', '', '', 'NumIdEst'))
#' VarList <- list(data.table(IDQual = c('NumIdEst','','','',''),
#'                      NonIDQual = c('EsMercNac', 'EsMercEuro', 'EsMercRM','Cod',''),
#'                      IDDD = c('','','','','IEPEntradaPed'),
#'                      Unit1 = c('','','','','')))
#' VarNameCorresp <- new(Class = 'VarNameCorresp', VarNameCorresp = VarList)
#' new(Class = 'DD', Data = DDData, VarNameCorresp = VarNameCorresp)
#' 
#' @include ExtractNames.R
#' 
#' @import data.table VarNameCorresp
#' 
#' @export
setClass(Class = "DD",
         slots = c(Data = 'data.table', VarNameCorresp = 'VarNameCorresp'),
         prototype = list(Data = data.table(Variable = character(0),
                                            Sort = character(0),
                                            Class = character(0),
                                            Qual1 = character(0)),
                          VarNameCorresp = new(Class = 'VarNameCorresp')),
         
         validity = function(object){
             
             ColNames <- names(object@Data)
             
             if (ColNames[1] != 'Variable'){
                 stop('[Validación DD] La primera columna del slot Data debe llamarse Variable.')
             }
             if (any(duplicated(object@Data[['Variable']]))){
                 stop('[Validación DD] La columna Variable del slot Data no puede tener valores repetidos.')
             }
             setkeyv(object@Data, 'Variable')
             
             if (ColNames[2] != 'Sort'){
                 stop('[Validación DD] La segunda columna del slot Data debe llamarse Sort.')
             }
             if (length(object@Data[['Sort']]) != 0 && !all(object@Data[['Sort']] %in% c('IDQual', 'NonIDQual', 'IDDD'))){ 
                 stop('[Validación DD] La columna Sort del slot Data sólo puede tener los valores IDQual, NonIDQual e IDDD.')
             }
             
             if (ColNames[3] != 'Class'){
                 stop('[Validación DD] La tercera columna del slot Data debe llamarse Class.')
             }
             
             if (ColNames[4] != 'Qual1'){
                 stop('[Validación DD] La cuarta columna del slot Data debe llamarse Qual1.')
             }
             
             if (!all(object@Data[['Variable']] == ExtractNames(object@Data[['Variable']]))){
                 stop('[Validación DD] Hay nombres inválidos de variables en la columna Variable del slot Data.')
             }
             
             Quals <- setdiff(ColNames, c('Variable', 'Sort', 'Class'))
             if (!all(Quals == paste0('Qual', seq(along = Quals)))){
                 stop('[Validación DD] Las columnas cuarta en adelante deben llamarse Qual1, Qual2, ...')
             }
             
             return(TRUE)
         }
)