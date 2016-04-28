#' @title Constructor of objects of class \linkS4class{VarNameCorresp}
#'
#' @description This constructor returns objects of class \linkS4class{VarNameCorresp}.
#' THe input parameter is a \code{list} of objects of class \linkS4class{VNCdt}.
#'
#' @param Data \code{List} of objects of class \linkS4class{VNCdt}.
#'
#' @return An object of class \linkS4class{VarNameCorresp}.
#'
#' @examples
#' library(data.table)
#' VarList <- list(
#'   ID = new(Class = 'VNCdt', 
#'            .Data = data.table(
#'                  IDQual = c('NumIdEst', rep('', 4)),
#'                  NonIDQual = rep('', 5),
#'                  IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                  NumIdEst = c('', rep('.', 4)),
#'                  Unit1 = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'))),
#'   MicroData = new(Class = 'VNCdt', 
#'                   .Data = data.table(
#'                       IDQual = c('NumIdEst', rep('', 4)),
#'                       NonIDQual = c('', 'IsNatMarket', 'IsEuroMarket', 'IsRWMarket', ''),
#'                       IDDD = c(rep('', 4), 'NewOrders'),
#'                       NumIdEst = c(rep('', 4), '.'),
#'                       IsNatMarket = c(rep('', 4), '0'),
#'                       IsEuroMarket = c(rep('', 4), '0'),
#'                       IsRWMarket = c(rep('', 4), '1'),
#'                       Unit1 = c('numidest', rep('', 3), 'cp09'))),
#'  Aggregates = new(Class = 'VNCdt', 
#'                   .Data = data.table(
#'                      IDQual = c('Province', 'NACE', 'IsNatMarket', ''),
#'                      NonIDQual = rep('', 4),
#'                      IDDD = c('', '', '', 'TotalTurnover'),
#'                      Province = c('', '', '', '.'),
#'                      NACE = c('', '', '', '.'),
#'                      IsNatMarket = c('', '', '', '1'),
#'                      Unit1 = c('provincia', 'actividad', '', 'cn01'))))
#' new(Class = 'VarNameCorresp', .Data = VarList)
#'
#' VNC <- BuildVNC(VarList)
#' VNC
#' #Notice that it is indeed an object with complex structure:
#' str(VNC)
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#'
#' @export
BuildVNC <- function(Data){
    
    if (is.null(names(Data))) stop('[StQ::BuildVNC] Data must be a named list of VNcdt objects.')
    
    VNC <- new(Class = 'VarNameCorresp')
    
    if (is.null(Data$ID)) Data$ID <- new(Class = 'VNCdt')
    
    if (is.null(Data$MicroData)) Data$MicroData <- new(Class = 'VNCdt')
    
    if (is.null(Data$ParaData)) Data$ParaData <- new(Class = 'VNCdt')
    
    
    out <- new(Class = 'VarNameCorresp', .Data = Data)
    validObject(out)
    
    return(out)
}
