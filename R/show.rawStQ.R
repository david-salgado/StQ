#' Show an object of class \linkS4class{StQ}
#'
#' \code{show} displays the slot \code{Data} of the input \linkS4class{StQ} object limiting the
#' number of columns on screen up to 8.
#'
#' This method displays only the content of slot \code{Data} from the input \linkS4class{StQ} 
#' object. It is indeed the method \link[methods]{show} adapted to class \linkS4class{StQ}.
#'
#' @param object Object of class \linkS4class{StQ}.
#'
#' @return Invisible \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'StQ'))
#' data(ExampleStQ)
#' show(ExampleStQ)
#' ExampleStQ
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

print.rawStQ <- function(object){show(object)}
