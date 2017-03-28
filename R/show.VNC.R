#' @title Show an object of class \linkS4class{VNC}
#'
#' @description The method \code{show} shows the slots of an object \linkS4class{VNC}
#' limiting the number of columns on screen up to 10.
#'
#' It is indeed the method \link[methods]{show} adapted to the class \linkS4class{VNC}.
#'
#' @param object Object of class \linkS4class{VNC}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(VNC())
#'
#' # A more elaborate example
#' library(data.table)
#' VarList <- list(
#'   ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                   NonIDQual = rep('', 5),
#'                   IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                   NumIdEst = c('', rep('.', 4)),
#'                   UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                   InFiles = rep('FI', 5)),
#'   MicroData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                          NonIDQual = c('', 'Market', ''),
#'                          IDDD = c(rep('', 2), 'NewOrders'),
#'                          NumIdEst = c(rep('', 2), '.'),
#'                          Market = c(rep('', 2), '1.'),
#'                          UnitName = c('numidest', '', 'cp09'),
#'                          InFiles = rep('FF', 3)),
#'  ParaData = data.table(IDQual = character(0), 
#'                        NonIDQual = character(0), 
#'                        IDDD = character(0),
#'                        UnitName = character(0),
#'                        InFiles = character(0)),
#'  Aggregates = data.table(IDQual = c('Province', 'NACE', 'Market', ''),
#'                          NonIDQual = rep('', 4),
#'                          IDDD = c('', '', '', 'TotalTurnover'),
#'                          Province = c('', '', '', '.'),
#'                          NACE = c('', '', '', '.'),
#'                          Market = c('', '', '', '2.'),
#'                          UnitName = c('provincia', 'actividad', '', 'cn01'),
#'                          InFiles = rep('FA', 4)))
#' VNC <- BuildVNC(VarList)
#' show(VNC)
#'
#' @include VNC.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "show",
    signature = c("VNC"),
    function(object){
        
        lapply(names(object), function(Name){
            
            cat(paste('\n', Name, '\n\n'))    
            ColMax <- 8 
            NamesCol <- names(object[[Name]])
            NumCol <- length(NamesCol)
            
            if (NumCol <= ColMax) {
                
                show(object[[Name]])
                
            } else {
                
                NumCols <- min(NumCol, ColMax)
                NamesShowCol <- NamesCol[c(1:(ColMax - 2), (NumCol - 1):NumCol)]
                show(object[[Name]][, NamesShowCol, with = F])
                cat('\n\n')
                cat(paste(rep('=', 40)), '\n\n')
                cat(paste0('The following columns have been omitted for clarity:\n ', paste0(setdiff(NamesCol, NamesShowCol), collapse = ', '),'\n'))
                cat('\n\n')
                cat(paste(rep('=', 40)), '\n\n')
            }    
        }
        )
        
        invisible(NULL)            
    }
    
)

#' @export
print.VNC <- function(object){show(object)}
