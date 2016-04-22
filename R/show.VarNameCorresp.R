#' @title Show an object of class \linkS4class{VarNameCorresp}
#'
#' @description The method \code{show} shows the slots of an object
#' \linkS4class{VarNameCorresp} limiting the number of columns on screen up to 
#' 10.
#'
#' It is indeed the method \link[methods]{show} adapted to the class
#' \linkS4class{VarNameCorresp}.
#'
#' @param Object of class \linkS4class{VarNameCorresp}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'VarNameCorresp'))
#'
#' # A more elaborate example
#' VarList <- list(MicroData = data.table(IDQual = c('NumIdEst','','', '', '', '', ''),
#'                 NonIDQual = c('', 'IsNatMarket', 'IsEuroMarket', 'IsRWMarket', '', '', ''),
#'                 IDDD = c('','','','','Turnover', 'Turnover', 'Turnover'),
#'                 NumIdEst = c('', '', '', '', '.', '.', '.'),
#'                 IsNatMarket = c('', '', '', '', '1', '0', '0'),
#'                 IsEuroMarket = c('', '', '', '', '', '1', '0'),
#'                 IsRWMarket = c('', '', '', '', '', '', '1'),
#'                 Unit1 = c('','','','','cn01', 'cn02', 'cn03')))
#' Example <- new(Class = 'VarNameCorresp', VarList)
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
        
        ColMax <- 10 
        lapply(names(object), function(Name){
            
            NamesCol <- names(Name)
            if (length(NamesCol) <= ColMax) {
                
                cat(paste('\n', Name, '\n\n'))    
                show(object[[Name]])
                    
            } else {
                    
                NumCols <- min(length(NamesCol), ColMax)
                NamesShowCol <- NamesCol[1:NumCols]
                cat(paste('\n', Name, '\n\n'))
                show(object[[Name]][, NamesShowCol, with = F])
                cat('\n\n')
                cat(paste(rep('=', 40)), '\n\n')
                    cat(paste0('The following columns have been omitted for clarity:\n ', paste0(setdiff(NamesCol, NamesShowCol), collapse = ', '),'\n'))
                    cat(paste(rep('=', 40)), '\n\n')
            }
        })
        
        invisible(NULL)
    }
)
