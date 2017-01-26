#' @title Dimensions of an Object
#'
#' @description Retrieve or set the dimension of an object.
#'
#' It is indeed the method \code{dim} for the classes \linkS4class{rawStQ} and \linkS4class{StQ}. 
#' This method returns the dimensions of the slot \code{Data} from the input object.
#'
#' @param x Object of class \linkS4class{rawStQ} or \linkS4class{StQ}.
#'
#' @return dim retrieves the dim attribute of the slot \code{Data} of the input object.
#'
#' @examples
#' data(ExampleStQ)
#' dim(ExampleStQ)
#' 
#' data(ExamplerawStQ)
#' dim(ExamplerawStQ)
#'
#' @include StQ-class.R getData.R 
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "dim",
    signature = c("StQ"),
    function(x){
        
        mc <- match.call()
        mc[['x']] <- getData(x)
        output <- eval(mc, envir = parent.frame())
        return(output)
        
    }
)

#' @rdname dim
#'
#' @include rawStQ-class.R getData.R 
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "dim",
    signature = c("rawStQ"),
    function(x){
        
        mc <- match.call()
        mc[['x']] <- getData(x)
        output <- eval(mc, envir = parent.frame())
        return(output)
        
    }
)
