#' @title Show an object of class \linkS4class{DD}
#'
#' @description The method \code{show} shows the slots of an object
#' \linkS4class{DD} limiting the number of columns on screen up to 8.
#'
#' It is indeed the method \link[methods]{show} adapted to the class
#' \linkS4class{DD}.
#'
#' @param object Object of class \linkS4class{DD}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'DD'))
#'
#' # A more elaborate example. See StQ-class and method getDD for details
#' data(ExampleQ)
#' show(getDD(ExampleQ))
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "show",
    signature = c("DD"),
    function(object){

      for (Slot in slotNames(object)){
        NamesCol <- names(slot(object, Slot))
        if (length(NamesCol) <= 8) {

            cat(paste0('Slot ', Slot, '\n\n'))
            show(slot(object, Slot))
            cat('\n\n')
            
        } else {

            NumCols <- min(length(NamesCol), 8)
            NamesShowCol <- NamesCol[1:NumCols]
            cat(paste0('Slot ', Slot, '\n'))
            show(slot(object, Slot)[, NamesShowCol, with = F])
            cat('\n\n')
            cat(paste(rep('=', 40)), '\n\n')
            cat(paste0('The following columns have been omited:\n ',
                       paste0(setdiff(NamesCol, NamesShowCol), collapse = ', '),
                       '.\n\n'))
            cat(paste(rep('=', 40)), '\n\n')
        }
      }
      invisible(NULL)
    }
)

