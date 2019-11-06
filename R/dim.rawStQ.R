#' @title Dimensions of an Object
#'
#' @description Retrieve or set the dimension of an object.
#'
#' It is indeed the method \code{dim} for the classes \linkS4class{rawStQ} and \link{StQ}. 
#' This method returns the dimensions of the slot \code{Data} from the input object.
#'
#' @param x Object of class \linkS4class{rawStQ} or \link{StQ}.
#'
#' @return dim retrieves the dim attribute of the slot \code{Data} of the input object.
#'
#' @examples
#' data(ExamplerawStQ)
#' dim(ExamplerawStQ)
#'
#' @include rawStQ.R getData.R 
#'
#' @import data.table
#'
#' @export
`dim.rawStQ` <- function(x){
    
    output <- dim(getData(x))
    return(output)
    
}
