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
#' @export
setGeneric("getDD", function(object) {standardGeneric("getDD")})
#'
#' @rdname getDD
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "getDD",
  signature = c("StQ"),
  function(object){

     return(copy(object@DD))

  }
)

#' @rdname getDD
#'
#' @include rawStQ-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getDD",
    signature = c("rawStQ"),
    function(object){
        
        return(copy(object@DD))
        
    }
)
#' @rdname getDD
#' 
#' @include StQList-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getDD",
    signature = c("StQList"),
    function(object){
        
        output <- lapply(getData(object), function(x) getDD(x))  
        
        return(output)
    }
)

#' @rdname getDD
#' 
#' @include rawStQList-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getDD",
    signature = c("rawStQList"),
    function(object){
        
        output <- lapply(getData(object), function(x) getDD(x))  
        
        return(output)
    }
)

