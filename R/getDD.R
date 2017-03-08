#' @title Return slot DD from the input object
#'
#' @description \code{getDD} extracts the slot \code{DD} of the input object.
#'
#' @param object Object of class \linkS4class{StQ}, \linkS4class{rawStQ}, \linkS4class{StQList} or 
#' \linkS4class{rawStQList}.
#'
#' @return Object of class \linkS4class{DD} corresponding to the slot \code{DD} of the input 
#' parameter.
#'
#' @examples
#' data(ExampleStQ)
#' DD <- getDD(ExampleStQ)
#' DD
#' str(DD)
#' 
#' @include DD.R StQ.R rawStQ.R getPeriods.R
#'
#' @import data.table

#' @export
setGeneric("getDD", function(object) {standardGeneric("getDD")})

#' @rdname getDD
#'
#' @export
setMethod(
  f = "getDD",
  signature = c("StQ"),
  function(object){object$DD}
)

#' @rdname getDD
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getDD",
    signature = c("rawStQ"),
    function(object){object$DD}
)

#' @rdname getDD
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getDD",
    signature = c("StQList"),
    function(object){
        
        output <- lapply(getDD, object$Data)
        names(output) <- getPeriods(object)
        return(output)
    }
)
