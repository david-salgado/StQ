#' @title Show an object of class \linkS4class{DDdt}
#'
#' @description The method \code{show} shows the slots of an object
#' \linkS4class{DDdt} limiting the number of columns on screen up to 8.
#'
#' It is indeed the method \link[methods]{show} adapted to the class
#' \linkS4class{DDdt}.
#'
#' @param object Object of class \linkS4class{DDdt}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' library(data.table)
#' show(new(Class = 'DDdt'))
#' 
#' DDdt1 <- new(Class = 'DDdt', data.table(Variable = 'NOrden', 
#'                                         Sort = 'IDQual', 
#'                                         Class = 'character',
#'                                         Length = '11',
#'                                         Qual1 = 'NOrden',
#'                                         ValueRegExp = '[0-9]{9}SS'))
#' show(DDdt1)
#'                         
#' DDdt2 <- new(Class = 'DDdt', data.table(Variable = 'NOrden', 
#'                                         Sort = 'IDQual', 
#'                                         Class = 'character',
#'                                         Length = '11',
#'                                         Qual1 = 'NOrden',
#'                                         Qual2 = 'Cal2',
#'                                         ValueRegExp = '[0-9]{9}SS'))
#' show(DDdt2)
#' 
#' @include DDdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "show",
    signature = c("DDdt"),
    function(object){

        ColMax <- 8 
        NamesCol <- names(object)
        
        if (length(NamesCol) <= ColMax) {
            
            mc <- match.call()
            New.object <- object@.Data
            names(New.object) <- object@names
            New.object <- setDT(New.object)
            return(New.object)
            
        } else {

            NumCols <- min(length(NamesCol), ColMax)
            NamesShowCol <- NamesCol[1:NumCols]
            show(object[, NamesShowCol, with = F])
            cat('\n\n')
            cat(paste(rep('=', 40)), '\n\n')
            cat(paste0('The following columns have been omitted for clarity:\n ', paste0(setdiff(NamesCol, NamesShowCol), collapse = ', '),'\n'))
            cat(paste(rep('=', 40)), '\n\n')
            return(invisible(NULL))
        }
        
        
    }
    
)


