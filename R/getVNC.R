#' @title Return slot \code{VarNameCorresp} from an object
#'
#' @description \code{getVNC} extracts slot \code{VarNameCorresp} from an object
#'  of class \linkS4class{DD} or class \linkS4class{StQ}.
#'
#' This method returns an object of class \linkS4class{VarNameCorresp} from an 
#' object of class \linkS4class{DD} or \linkS4class{StQ} specified as input 
#' argument.
#'
#' @param Object of class \linkS4class{DD} or \linkS4class{StQ}.
#'
#' @return Object of class \linkS4class{VarNameCorresp} of the input parameter.
#'
#' @include VarNameCorresp-class.R DD-class.R StQ-class.R
#'
#' @examples 
#' # From an object of class DD
#' data(ExampleDD)
#' VNC <- getVNC(ExampleDD)
#' VNC
#' str(VNC)
#'
#'# From an object of class StQ
#' data(ExampleQ)
#' VNC <- getVNC(ExampleQ)
#' VNC
#' str(VNC)
#' 
#' @export
setGeneric("getVNC", function(object) {standardGeneric("getVNC")})

#' @rdname getVNC
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getVNC",
    signature = c("DD"),
    function(object){
        
        VNC <- copy(object@VarNameCorresp)
        return(VNC)
        
    }
)
#' @rdname getVNC
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getVNC",
    signature = c("StQ"),
    function(object){
        
        VNC <- getVNC(getDD(object))
        return(VNC)
        
    }
)
