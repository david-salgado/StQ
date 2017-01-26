#' @title Extract parts of an object of class \linkS4class{StQ}
#'
#' @description \code{[} extracts parts of an object of class \linkS4class{StQ}.
#'
#' It is indeed the method \code{[} for the class \linkS4class{StQ}. This method returns subsets of
#' the slot \code{Data} from an object of class \linkS4class{StQ} specified as an input parameter.
#' The output is an object of the same class \linkS4class{StQ} as the input parameter \code{x}.
#'
#' @param x Object of class \linkS4class{StQ}.
#'
#' @param i,j,... Indices corresponding to elements to be extracted. The indices are numeric or
#' character vectors, \code{\link{missing}} or \code{\link{NULL}}. Numeric values are coerced to
#' \code{integer} with \code{\link{as.integer}} (thus truncated to zero).
#'
#' @param drop Included by coherence.
#'
#' @return Object of class \linkS4class{StQ} with the subsetted input object.
#'
#' @examples
#' data(ExampleStQ)
#' ExampleStQ[IDDD == 'Turnover']
#' ExampleStQ[3:4]
#' ExampleStQ[ID == '00021']
#'
#' @include StQ-class.R getData.R setData.R Datadt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "[",
  signature = c("StQ"),
  function(x, i, j, ..., drop = TRUE){

    mc <- match.call()
    mc[['x']] <- getData(x)
    output <- x
    x.subsetted <- eval(mc, envir = parent.frame())
    Datadt <- new(Class = 'Datadt', x.subsetted)
    setData(output) <- new(Class = 'Datadt', Datadt)
    validObject(output)
    return(output)

  }
)
