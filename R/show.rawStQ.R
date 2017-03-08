<<<<<<< HEAD
#' Show an object of class \linkS4class{StQ}
#'
#' \code{show} displays the slot \code{Data} of the input \linkS4class{StQ} object limiting the
#' number of columns on screen up to 8.
#'
#' This method displays only the content of slot \code{Data} from the input \linkS4class{StQ} 
#' object. It is indeed the method \link[methods]{show} adapted to class \linkS4class{StQ}.
#'
#' @param object Object of class \linkS4class{StQ}.
=======
#' Show an object of class \linkS4class{rawStQ}
#'
#' \code{show} displays the slot \code{Data} of the input \linkS4class{rawStQ}.
#'
#' This method displays only the content of slot \code{Data} from the input \linkS4class{rawStQ}
#' object. It is indeed the method \link[methods]{show} adapted to class \linkS4class{rawStQ}.
#'
#' @param object Object of class \linkS4class{rawStQ}.
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @return Invisible \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
<<<<<<< HEAD
#' show(new(Class = 'StQ'))
#' data(ExampleStQ)
#' show(ExampleStQ)
#' ExampleStQ
#'
#' @include rawStQ.R getData.R
=======
#' show(new(Class = 'rawStQ'))
#'
#' show(ExamplerawStQ)
#'
#' @include rawStQ-class.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "show",
    signature = c("rawStQ"),
    function(object){
        
<<<<<<< HEAD
        show(getData(object))
=======
        show(object@Data)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        
        invisible(NULL)
    }
)
<<<<<<< HEAD

print.rawStQ <- function(object){show(object)}
=======
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
