#' @title Show an object of class \linkS4class{VarNameCorresp}
#'
#' @description The method \code{show} shows the slots of an object
#' \linkS4class{VarNameCorresp} limiting the number of columns on screen up to 
#' 10.
#'
#' It is indeed the method \link[methods]{show} adapted to the class
#' \linkS4class{VarNameCorresp}.
#'
#' @param object Object of class \linkS4class{VarNameCorresp}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'VarNameCorresp'))
#'
#' # A more elaborate example
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
#'  ParaData = new(Class = 'VNCdt'),
#'  Aggregates = new(Class = 'VNCdt', 
#'                   .Data = data.table(
#'                      IDQual = c('Province', 'NACE', 'IsNatMarket', ''),
#'                      NonIDQual = rep('', 4),
#'                      IDDD = c('', '', '', 'TotalTurnover'),
#'                      Province = c('', '', '', '.'),
#'                      NACE = c('', '', '', '.'),
#'                      IsNatMarket = c('', '', '', '1'),
#'                      Unit1 = c('provincia', 'actividad', '', 'cn01'))))
#' VNC2 <- new(Class = 'VarNameCorresp', .Data= VarList)
#' show(VNC2)
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "show",
    signature = c("VarNameCorresp"),
    function(object){
        
        lapply(names(object), function(Name){
            
            NamesCol <- names(Name)
            cat(paste('\n', Name, '\n\n'))    
            show(object[[Name]])    
        }
        )
        
        invisible(NULL)            
    }
    
)
