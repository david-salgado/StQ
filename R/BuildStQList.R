#' @title Constructor of objects of class \linkS4class{StQList}
#'
#' @description This constructor returns an object of class \linkS4class{StQList}.
#' The input parameter is a named \code{list} of objects of class \link{StQ}. Notice that 
#' the names of the \code{list} must be valid time period names according to package 
#' \link[=RepoTime]{RepoTime}.
#'
#' @param Data \code{List} of objects of class \link{StQ}.
#'
#' @return An object of class \linkS4class{StQList}.
#'
#' @examples
#' EmptyQ <- StQ()
#' EmptyQList <- vector('list', 12)
#' EmptyQList <- lapply(EmptyQList, function(x) EmptyQ)
#' mm <- c(paste0('0', 1:9), 10:12)
#' names(EmptyQList) <- paste0('MM', mm, '2015')
#' QList <- BuildStQList(EmptyQList)
#' QList
#' #Notice that it is indeed an object with complex structure:
#' str(QList)
#'
#' @include StQ.R StQList.R
#'
#' @export
BuildStQList <- function(Data){

    if (is.null(names(Data))) stop('[StQ::BuildStQList] Data must be a named list of StQ objects.\n')

    PeriodList <- RepoTime::newRepoTime(names(Data))

    out <- StQList(Data = Data, Periods = PeriodList)

    return(out)
}
