#' @title Show an object of class \linkS4class{Datadt}
#'
#' @description The method \code{show} shows the slots of an object
#' \linkS4class{Datadt} limiting the number of columns on screen up to 8.
#'
#' It is indeed the method \link[methods]{show} adapted to the class
#' \linkS4class{Datadt}.
#'
#' @param object Object of class \linkS4class{Datadt}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'Datadt'))
#' 
#' data(ExampleStQ)
#' show(getData(ExampleStQ))
#'                         
#' DDdt2 <- new(Class = 'DDdt', data.table(Variable = 'NOrden', 
#'                                         Sort = 'IDQual', 
#'                                         Class = 'character', 
#'                                         Qual1 = 'NOrden',
#'                                         Qual2 = 'Cal2',
#'                                         Qual3 = 'Cal4',
#'                                         Qual4 = 'Cal4',
#'                                         Qual5 = 'Cal5',
#'                                         Qual6 = 'Cal6',
#'                                         ValueRegExp = '[0-9]{9}SS'))
#' show(DDdt2)
#' 
#' @include Datadt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "show",
    signature = c("Datadt"),
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
            
        } else {
            
            
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


