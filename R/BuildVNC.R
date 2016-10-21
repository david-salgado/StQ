#' @title Constructor of objects of class \linkS4class{VarNameCorresp}
#'
#' @description This constructor returns objects of class \linkS4class{VarNameCorresp}.
#' THe input parameter is a \code{list} of objects of class \linkS4class{VNCdt}.
#'
#' @param Data \code{List} of named objects of class \linkS4class{VNCdt}.
#'
#' @return An object of class \linkS4class{VarNameCorresp} with components 
#' specified in the input Data. Components 'ID', 'MicroData' and/or 'ParaData' 
#' no specified are set as an empty \linkS4class{VNCdt} object.
#' 
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
#'                  UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'))),
#'   MicroData = new(Class = 'VNCdt', 
#'                   .Data = data.table(
#'                       IDQual = c('NumIdEst', rep('', 2)),
#'                       NonIDQual = c('', 'Market', ''),
#'                       IDDD = c(rep('', 2), 'NewOrders'),
#'                       NumIdEst = c(rep('', 2), '.'),
#'                       Market = c(rep('', 2), '1.'),
#'                       UnitName = c('numidest', '', 'cp09'))),
#'  Aggregates = new(Class = 'VNCdt', 
#'                   .Data = data.table(
#'                      IDQual = c('Province', 'NACE', 'Market', ''),
#'                      NonIDQual = rep('', 4),
#'                      IDDD = c('', '', '', 'TotalTurnover'),
#'                      Province = c('', '', '', '.'),
#'                      NACE = c('', '', '', '.'),
#'                      Market = c('', '', '', '1.'),
#'                      UnitName = c('provincia', 'actividad', '', 'cn01'))))
#'                      
#' VNC <- BuildVNC(VarList)
#' VNC
#' 
#' #Notice that it is indeed an object with complex structure:
#' str(VNC)
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#'
#' @export
BuildVNC <- function(Data){
    
    if (is.null(names(Data))) stop('[StQ::BuildVNC] Data must be a named list of VNCdt objects.')
    
    if (is.null(Data$ID)) Data$ID <- new(Class = 'VNCdt')
    
    if (is.null(Data$MicroData)) Data$MicroData <- new(Class = 'VNCdt')
    
    if (is.null(Data$ParaData)) Data$ParaData <- new(Class = 'VNCdt')
    
    Data <- Data[c('ID', 'MicroData', 'ParaData', setdiff(names(Data), c('ID', 'MicroData', 'ParaData' )))]
    out <- new(Class = 'VarNameCorresp', .Data = Data)
    validObject(out)
    
    return(out)
}
