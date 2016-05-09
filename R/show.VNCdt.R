#' @title Show an object of class \linkS4class{VNCdt}
#'
#' @description The method \code{show} shows the slots of an object
#' \linkS4class{VNCdt} limiting the number of columns on screen up to 
#' 10.
#'
#' It is indeed the method \link[methods]{show} adapted to the class
#' \linkS4class{VNCdt}.
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
#'               data.table(IDQual = c('NumIdEst', '', '', '', '', '', '', ''),
#'                          NonIDQual = c('', 'EsMercNac', 'EsMercEuro', 'EsMercRM',
#'                                                      'Cod','Var1', 'Var2', ''),
#'                          IDDD = c('', '', '', '', '', '', '','IEPEntradaPed'),
#'                          NumIdEst = c('', '', '', '', '', '', '', '.'),
#'                          EsMercNac = c('', '', '', '', '', '', '','0'),
#'                          EsMercEuro = c('', '', '', '', '', '', '','0'),
#'                          EsMercRM = c('', '', '', '', '', '', '','1'),
#'                          Cod = c('', '', '', '', '', '', '', ''),
#'                          Var1 = c('', '', '', '', '', '', '', ''),
#'                          Var2 = c('', '', '', '', '', '', '', ''),
#'                          Unit1 = c('', '', '', '', '', '', '','cp09')))
#' 
#'
#' VNCdt2 <- new(Class = 'VNCdt', 
#'               data.table(IDQual = c('NOrden', '', '', ''), 
#'                          NonIDQual = c('', 'IsNatMarket', '', ''), 
#'                          IDDD = c('', '', 'Turnover', 'Turnover'),
#'                          NOrden = c('', '', '.', '.'),
#'                          IsNatMarket = c('', '', '0', '1'),
#'                          Unit1 = c('', '', 'cn01', 'cn02')))
#'
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
            output <- eval(mc, envir = parent.frame())
            return(output)
            
                    
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
        

