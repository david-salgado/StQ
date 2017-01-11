#' @title Show an object of class \linkS4class{Datadt}
#'
#' @description The method \code{show} shows the slots of an object \linkS4class{Datadt} limiting 
#' the number of columns on screen up to 8.
#'
#' It is indeed the method \link[methods]{show} adapted to the class \linkS4class{Datadt}.
#'
#' @param object Object of class \linkS4class{Datadt}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' library(data.table)
#' # A trivial example
#' show(new(Class = 'Datadt'))
#' 
#' data(ExampleStQ)
#' show(getData(ExampleStQ))
#' 
#' Datadt <- new(Class = 'Datadt', 
#'               data.table(Qual1 = 'NOrden', 
#'                          Qual2 = '1.2.', 
#'                          Qual3 = '2.',
#'                          Qual4 = '0',
#'                          Qual5 = '10.',
#'                          Qual6 = ' ',
#'                          Qual7 = '1.',
#'                          Qual8 = '3.5.',
#'                          IDDD = 'Turnover',
#'                          Value = '12920'))
#' show(Datadt)
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
        NumCol <- length(NamesCol)
        if (NumCol <= ColMax) {
            
            mc <- match.call()
            New.object <- object@.Data
            names(New.object) <- object@names
            New.object <- setDT(New.object)
            mc[['object']] <- New.object
            eval(mc, envir = parent.frame())
            
        } else {
            
            NumCols <- min(NumCol, ColMax)
            NamesShowCol <- NamesCol[c(1:(ColMax - 2), (NumCol - 1):NumCol)]
            show(object[, NamesShowCol, with = F])
            cat('\n\n')
            cat(paste(rep('=', 40)), '\n\n')
            cat(paste0('The following columns have been omitted for clarity:\n ', paste0(setdiff(NamesCol, NamesShowCol), collapse = ', '),'\n'))
            cat(paste(rep('=', 40)), '\n\n')
        }
        
        invisible(NULL)
    }
    
)


