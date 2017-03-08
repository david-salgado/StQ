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
#' @include StQ.R getData.R setData.R VNC.R DD.R
#'
#' @import data.table
#'
#' @export
`[.StQ` <- function(x, i, j, by, keyby, with=TRUE, nomatch=getOption("datatable.nomatch"), mult="all", roll=FALSE, rollends=if (roll=="nearest") c(TRUE,TRUE) else if (roll>=0) c(FALSE,TRUE) else c(TRUE,FALSE), which=FALSE, .SDcols, verbose=getOption("datatable.verbose"), allow.cartesian=getOption("datatable.allow.cartesian"), drop=NULL, on=NULL){
    
    mc <- match.call()
    auxDT <- getData(x)
    mc[[1L]] <- data.table:::`[.data.table`
    mc[['x']] <- auxDT
    x.subsetted <- eval(mc, envir = auxDT, enclos = parent.frame())
    setData(x) <- x.subsetted
    return(x)
}

