#' Show an object of class \linkS4class{rawStQ}
#'
#' \code{show} displays the slot \code{Data} of the input \linkS4class{rawStQ} object limiting the
#' number of columns on screen up to 8.
#'
#' This method displays only the content of slot \code{Data} from the input \linkS4class{rawStQ} 
#' object. It is indeed the method \link[methods]{show} adapted to class \linkS4class{rawStQ}.
#'
#' @param object Object of class \linkS4class{rawStQ}.
#'
#' @return Invisible \code{\link{NULL}}.
#'
#' @examples
#' data(ExamplerawStQ)
#' show(ExamplerawStQ)
#'
#' @include rawStQ.R getData.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "show",
    signature = c("rawStQ"),
    function(object){
        
        show(getData(object))
        
        invisible(NULL)
    }
)

#' @export
print.rawStQ <- function(object){show(object)}
