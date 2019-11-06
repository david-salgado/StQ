#' @title Extract parts of an object of class \linkS4class{StQList}
#'
#' @description \code{[[} extracts parts of an object of class \linkS4class{StQList}.
#'
#' It is indeed the method \code{[[} for the class \linkS4class{StQList}. This method returns
#' objects of class \link{StQ} from the slot \code{Data} of the input object \code{x}.
#'
#' @param x object of class \linkS4class{StQList}.
#'
#' @param i,j,... Indices corresponding to the elements to be extracted. These indices are numeric
#' vector or character vector or \link{missing} or \link{NULL}. Numeric values are coerced
#' internally to \code{integer} through \code{\link{as.integer}} (and thus truncated to zero).
#' Character vector correspond to names of the respective time period of each component object of
#' class \link{StQ}.
#'
#' @param exact Included by coherence.
#'
#' @return Object of class \link{StQ}.
#'
#' @examples
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' PeriodList <- RepoTime::newRepoTime(TimePer)
#' EmptyQ <- StQ()
#' EmptyQList <- vector('list', 12)
#' EmptyQList <- lapply(EmptyQList, function(x) EmptyQ)
#' QList <- StQList(Data = EmptyQList, Periods = PeriodList)
#' QList[['MM092015']]
#' QList[[2]]
#'
#' @include StQList.R getPeriods.R getData.R
#'
#' @import data.table
#'
#' @export
`[[.StQList` <- function(x, i, j, ..., exact = TRUE){


    mc <- match.call()
    New.x <- x$Data
    Periods <- getPeriods(x)
    names(New.x) <- Periods
    mc[[1L]] <- `[[`
    mc[['x']] <- New.x
    output <- eval(mc, x, parent.frame())
    return(output)

}

