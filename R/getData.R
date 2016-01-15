#' Obtiene el slot Data del objeto pasado como parámetro.
#'
#' \code{getData} devuelve el slot \code{Data} del objeto especificado como 
#' argumento de entrada.
#' 
#' En el caso de objetos \linkS4class{StQ} devuelve un conjunto de datos 
#' restringido a los valores de la columna \code{IDDD} del slot \code{Data}
#' indicados en el parámetro de entrada \code{VarNames}.
#' 
#' Los objetos de entrada pueden tener las clases:
#' \itemize{
#' \item \code{StQ}: En estos casos esta función toma como input un objeto de 
#' clase \linkS4class{StQ} y un vector, \code{VarNames}, con nombres de 
#' variables y devuelve la \code{data.table} correspondiente al slot \code{Data}
#' de dicho objeto \code{StQ}, pero sólo con los datos relativos a las 
#' variables indicadas en \code{VarNames}.
#'
#' Si no se especifica ninguna variable en \code{VarNames} la función devuelve 
#' el slot \code{Data} del objeto \code{StQ} de entrada al completo.
#' 
#' \item \code{DD}: En estos casos esta función toma como input un objeto de 
#' clase \linkS4class{DD}. El parámetro \code{VarNames} no tiene efecto sobre la 
#' ejecución.
#' }
#' 
#' @param object Objeto cuyo slot \code{Data} restringido a las variables 
#' especificadas quiere obtenerse.
#'
#' @param VarNames \code{\link{vector}} de clase \code{character} con los nombres
#' de las variables a extraer. Por defecto, incluye todas las variables en el
#' slot \code{Data} del objeto \code{StQ}.
#'
#' @return En el caso de objetos \linkS4class{StQ} devuelve una 
#' \linkS4class{data.table} con estructura par clave-valor correspondiente al 
#' slot \code{Data} del objeto de entrada \code{StQ}, con 
#' los valores de la columna \code{IDDD} restringidos a la variables en \code{VarNames}.
#'
#' @examples
#' # De un objeto DD
#' DDData <- data.table(Variable = c('NumIdEst', 'EsMercNac', 'EsMercEuro', 'EsMercRM', 'Cod', 'IEPEntradaPed'),
#'                      Sort = c('IDQual', 'NonIDQual', 'NonIDQual', 'NonIDQual', 'NonIDQual', 'IDDD'),
#'                      Class = c('character', 'character', 'character', 'character', 'character', 'character'),
#'                      Qual1 = c('', '', '', '', '', 'NumIdEst'))
#' VarList <- list(data.table(IDQual = c('NumIdEst','','','',''),
#'                      NonIDQual = c('EsMercNac', 'EsMercEuro', 'EsMercRM','Cod',''),
#'                      IDDD = c('','','','','IEPEntradaPed'),
#'                      Unit1 = c('','','','','')))
#' VarNameCorresp <- new(Class = 'VarNameCorresp', VarNameCorresp = VarList)
#' DD <- new(Class = 'DD', Data = DDData, VarNameCorresp = VarNameCorresp)
#' getData(DD)
#' 
#' 
#' # De un objeto StQ
#' VarNames <- c('IASSCifraNeg', 'IASSEmpleo')
#' getData(ExampleQ, VarNames)
#' 
#' VarNames <- c('IASSCifraNeg', 'IASSEmpleo_0')
#' getData(ExampleQ, VarNames)
#'
#' # De un objeto StQList
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' QList <- vector('list', 12)
#' QList <- lapply(QList, function(x) ExampleQ)
#' names(QList) <- TimePer
#' QList <- new(Class = 'StQList', QList)
#' VarNames <- c('IASSCifraNeg', 'IASSEmpleo')
#' getData(QList, VarNames)
#' 
#' @export
setGeneric("getData", function(object, VarNames){standardGeneric("getData")})
#' @rdname getData
#' 
#' @include DD-class.R
#' 
#' @export
setMethod(
  f = "getData",
  signature = c("DD"),
  function(object, VarNames){
    
    out <- object@Data
    return(out)
  }
)
#' @rdname getData
#' 
#' @include StQ-class.R ExtractNames.R VarNamesToDD.R getDD.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
  f = "getData",
  signature = c("StQ"),
  function(object, VarNames){
    
    
    if (missing(VarNames)) return(object@Data)
    
    VarNames.DT <- VarNamesToDD(VarNames, getDD(object))
    setkeyv(VarNames.DT, names(VarNames.DT))
    DataNames <- names(object@Data)
    setkeyv(object@Data, names(VarNames.DT))
    output <- merge(getData(object), VarNames.DT)
    setcolorder(output, DataNames)
    if(dim(output)[1] == 0) {
      
      warning('[getData StQ] No existen tales variables en el conjunto de datos.')
      return(output)
    }
    
    for (col in sort(names(output))){
      
      if (all(output[[col]] == '')) output[, col := NULL, with = F]
      
    }
    
    NotPresent <- VarNames[which(!ExtractNames(VarNames) %in% unique(getData(object)[['IDDD']]))]
    
    if (length(NotPresent) > 0){
      
      warning(paste0('[getData StQ] Las siguientes variables no se encuentran en el conjunto de datos: ', 
                     paste0(NotPresent, collapse = ', '),
                     '.\n No se incluyen en la data.table de salida.'))
      
    }
    
    return(output)
  }
)
#' @rdname getData
#' 
#' @include StQList-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
  f = "getData",
  signature = c("StQList"),
  function(object, VarNames){
    
    if (missing(VarNames)){
      
      output <- lapply(object@Data,function(x) getData(x))
      
      return(output)
    }
    
    output <- lapply(object@Data,function(x) getData(x,VarNames))  
    
    return(output)
  }
)