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
<<<<<<< HEAD
#' @include StQ.R getData.R 
=======
#' @include StQ-class.R getData.R 
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table
#'
#' @export
<<<<<<< HEAD
`dim.StQ` <- function(x){
    
    output <- dim(getData(x))
    return(output)
        
}

=======
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
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
