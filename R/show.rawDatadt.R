#' @title Show an object of class \linkS4class{rawDatadt}
#'
#' @description The method \code{show} shows the slot \code{Data} of an object \linkS4class{rawDatadt}.
#'
#' It is indeed the method \link[methods]{show} adapted to the class \linkS4class{rawDatadt}.
#'
#' @param object Object of class \linkS4class{rawDatadt}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'rawDatadt'))
#'
#' show(getData(ExamplerawStQ))
#' 
#' @include DDdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "show",
    signature = c("rawDatadt"),
    function(object){
        
        mc <- match.call()
        New.object <- object@.Data
        names(New.object) <- object@names
        New.object <- setDT(New.object)
        show(New.object)
        return(invisible(NULL))
            
    }
)


