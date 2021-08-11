#' @title Show an object of class \link{StQ}
#'
#' @description \code{show} displays the slot \code{Data} of the input \link{StQ} object limiting the
#' number of columns on screen up to 8.
#'
#' This method displays only the content of slot \code{Data} from the input \link{StQ} 
#' object. It is indeed the method \link[methods]{show} adapted to class \link{StQ}.
#'
#' @param object Object of class \link{StQ}.
#'
#' @return Invisible \code{\link{NULL}}.
#'
#' @examples
#' data(ExampleStQ)
#' show(ExampleStQ)
#' ExampleStQ
#' 
#'
#' @include StQ.R getData.R
#'
#' @import data.table methods
#'
#' @export
setMethod(
    f = "show",
    signature = c("StQ"),
    function(object){
        
        ColMax <- 8 
        NamesCol <- names(getData(object))
        NumCol <- length(NamesCol)
        if (NumCol <= ColMax) {

            show(getData(object))

        } else {
            
            NumCols <- min(NumCol, ColMax)
            NamesShowCol <- NamesCol[c(1:(ColMax - 2), (NumCol - 1):NumCol)]
            show(getData(object)[, NamesShowCol, with = F])
            cat('\n')
            cat(paste(rep('=', 40)), '\n\n')
            cat(paste0('[StQ::show] The following columns have been omitted:\n ',
                       paste0(setdiff(NamesCol, NamesShowCol), collapse = ', '),
                       '.\n'))
            cat(paste(rep('=', 40)), '\n\n')
        }

        invisible(NULL)
    }
)

#' @export
print.StQ <- function(x, ...){show(x)}
