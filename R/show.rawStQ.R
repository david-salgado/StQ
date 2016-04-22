#' Show an object of class \linkS4class{rawStQ}.
#'
#' \code{show} displays the slot \code{Data} of the input \linkS4class{rawStQ}.
#'
#' This method displays only the content of slot \code{Data} from the input
#' \linkS4class{rawStQ} object. It is indeed the method \link[methods]{show} 
#' adapted to class \linkS4class{rawStQ}.
#'
#' @param object Object of class \linkS4class{rawStQ}.
#'
#' @return Invisible \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'rawStQ'))
#'
#' @include rawStQ-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "show",
    signature = c("rawStQ"),
    function(object){
        
        show(object@Data)
        
        invisible(NULL)
    }
)
