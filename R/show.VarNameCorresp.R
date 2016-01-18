#' @title Show an object of class \linkS4class{VarNameCorresp}
#'
#' @description The method \code{show} shows the slots of an object
#' \linkS4class{VarNameCorresp} limiting the number of columns on screen up to 8.
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
#' # A more elaborate example. See StQ-class and method getDD for details
#' VarList <- list(data.table(IDQual = c('NumIdEst','','','',''),
#'                      NonIDQual = c('EsMercNac', 'EsMercEuro', 'EsMercRM','Cod',''),
#'                      IDDD = c('','','','','IEPEntradaPed'),
#'                      Unit1 = c('','','','','')))
#' Example <- new(Class = 'VarNameCorresp', VarNameCorresp = VarList)
#' show(Example)
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
        
        for (Slot in slotNames(object)){
            lapply(slot(object, Slot), function(x){
                NamesCol <- names(x)
                if (length(NamesCol) <= 8) {
                    
                    cat(paste0('Slot ', Slot, '\n'))
                    show(x)
                    
                } else {
                    
                    NumCols <- min(length(NamesCol), 8)
                    NamesShowCol <- NamesCol[1:NumCols]
                    cat(paste0('Slot ', Slot, '\n'))
                    show(x[, NamesShowCol, with = F])
                    cat('\n')
                    cat(paste(rep('=', 40)), '\n\n')
                    cat(paste0('The following columns have been omitted for clarity:\n ', paste0(setdiff(NamesCol, NamesShowCol), collapse = ', '),'\n'))
                    cat(paste(rep('=', 40)), '\n\n')
                }
            })
        }
        
        invisible(NULL)
    }
)