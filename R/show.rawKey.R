#' @title Show an object of class \linkS4class{rawKey}
#'
#' @description The method \code{show} displays an object \linkS4class{rawKey}.
#'
#' It is indeed the method \link[methods]{show} adapted to the class \linkS4class{rawKey}.
#'
#' @param object Object of class \linkS4class{rawKey}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'rawKey'))
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
    signature = c("rawKey"),
    function(object){
        
        mc <- match.call()
        New.object <- object@.Data
        names(New.object) <- object@names
        New.object <- setDT(New.object)
        show(New.object)
        return(invisible(NULL))
        
    }
)


