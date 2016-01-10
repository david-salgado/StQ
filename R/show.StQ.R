#' Show an object of class \linkS4class{StQ}
#'
#' \code{show} displays the slot \code{Data} of the input \linkS4class{StQ}
#' object limiting the number of columns on screen up to 8.
#'
#' This method displays only the content of slot \code{Data} from the input
#' \linkS4class{StQ} object. It is indeed the method \link[methods]{show} adapted
#' to class \linkS4class{StQ}.
#'
#' @param object Object of class \linkS4class{StQ}.
#'
#' @return Invisible \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'StQ'))
#' data(ExampleQ)
#' show(ExampleQ)
#' ExampleQ
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "show",
    signature = c("StQ"),
    function(object){

        NamesCol <- names(getData(object))
        if (length(NamesCol) <= 10) {

            show(getData(object))

        } else {

            NumCols <- min(length(NamesCol) - 2, 8)
            NamesShowCol <- c(NamesCol[1:NumCols], 'IDDD', 'Value')
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

