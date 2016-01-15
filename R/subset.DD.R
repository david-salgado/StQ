#' Extrae partes de un objeto de clase DD.
#' 
#' \code{[} extrae o reemplaza partes de un objeto de clase \linkS4class{DD}.
#' 
#' Se trata del método \code{[} para la clase \linkS4class{DD}. Este método 
#' obtiene subconjuntos del objeto de clase \code{DD} especificado como entrada
#'  en base a su slot \code{Data}. Devuelve, por tanto, un objeto de la misma 
#'  clase \code{DD} del que se extrae un determinado subconjunto del slot 
#'  \code{Data}.
#' 
#' @param x objeto de clase \linkS4class{DD} del que se van a extraer los 
#' elementos.
#'
#' @param i,j, ... índices correspondientes a los elementos a extraer. 
#' Los índices son \code{vectores} de clase \code{numeric} o de clase 
#' \code{character} o de clase \code{\link{missing}} o \code{\link{NULL}}. Los 
#' valores \code{numeric} son forzados a \code{integer} mediante
#' \code{\link{as.integer}} (y por tanto, truncados hacia cero). Los vectores de
#'  clase \code{character} corresponderán a los nombres de los objetos (o para 
#'  matrices/arrays, los dimnames).
#'       
#' @param drop Incluido por coherencia.
#'
#' @return Objeto de clase \linkS4class{DD}, que consiste en un subconjunto del
#'  objeto \code{DD} de entrada.
#'  
#' @examples
#' data(ExampleDD)
#' ExampleDD[Variable == 'IASSCifraNeg']
#' 
#' @include StQ-class.R getData.R setData.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
  f = "[",
  signature = c("DD"),
  function(x, i, j, ..., drop = TRUE){
    
    mc <- match.call()
    mc[['x']] <- getData(x)
    subDD <- eval(mc, envir = parent.frame())
    output <- new(Class = 'DD', Data = subDD)
    return(output)
    
  }
)