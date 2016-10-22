#' @title Show an object of class \linkS4class{VNCdt}
#'
#' @description The method \code{show} shows the slots of an object \linkS4class{VNCdt} limiting the
#' number of columns on screen up to 8.
#'
#' It is indeed the method \link[methods]{show} adapted to the class \linkS4class{VNCdt}.
#'
#' @param object Object of class \linkS4class{VNCdt}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'VNCdt'))
#' 
#' library(data.table)
#' VNCdt1 <- new(Class = 'VNCdt',
#'               data.table(IDQual = c('NumIdEst', '', '', ''),
#'                          NonIDQual = c('', 'Market', 'Cod', ''),
#'                          IDDD = c('', '', '','NewOrders'),
#'                          NumIdEst = c('', '', '', '.'),
#'                          Market = c( '', '', '','0'),
#'                          Cod = c('', '', '', ''),
#'                          UnitName = c( '', '', '','cp09'),
#'                          InFiles = rep('FF', 4)))
#' show(VNCdt1)
#'
#' VNCdt2 <- new(Class = 'VNCdt', 
#'               data.table(IDQual = c('NOrden', '', ''), 
#'                          NonIDQual = c('', 'Market', ''), 
#'                          IDDD = c('', '', 'Turnover'),
#'                          NOrden = c('', '', '.'),
#'                          Market = c('', '', '1.'),
#'                          UnitName = c('', '', 'cn01'),
#'                          InFiles = rep('FF', 3)))
#'show(VNCdt2)
#'
#' @include VNCdt-class.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "show",
    signature = c("VNCdt"),
    function(object){
        
        ColMax <- 8 
        NamesCol <- names(object)
        
        if (length(NamesCol) <= ColMax) {
            
            mc <- match.call()
            New.object <- object@.Data
            names(New.object) <- object@names
            New.object <- setDT(New.object)
            mc[['object']] <- New.object
            eval(mc, envir = parent.frame())
                    
        }else {
                    
                NumCols <- min(length(NamesCol), ColMax)
                NamesShowCol <- NamesCol[1:NumCols]
                show(object[, NamesShowCol, with = F])
                cat('\n\n')
                cat(paste(rep('=', 40)), '\n\n')
                    cat(paste0('The following columns have been omitted for clarity:\n ', paste0(setdiff(NamesCol, NamesShowCol), collapse = ', '),'\n'))
                    cat(paste(rep('=', 40)), '\n\n')
            }
        invisible(NULL)
    }
    
)
        

