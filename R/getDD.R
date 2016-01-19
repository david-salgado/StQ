#' @title Return slot \code{DD} from an object of class \linkS4class{StQ}
#'
#' @description \code{getDD} extracts slot \code{DD} from an object of class
#'  \linkS4class{StQ}.
#'
#' This method returns the \linkS4class{data.table} corresponding to slot
#' \code{DD} from an object of class \linkS4class{StQ} specified as input
#' argument.
#'
#' @param Object of class \linkS4class{StQ}.
#'
#' @return Object of class \linkS4class{DD} corresponding to the slot \code{DD}
#' of the input parameter.
#'
#' @include StQ-class.R
#'
#' @examples
#' data(ExampleQ)
#' DD <- getDD(ExampleQ)
#' DD
#' str(DD)
#'
#' @export
setGeneric("getDD", function(object) {standardGeneric("getDD")})

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

     return(object@DD)

  }
)
