#' @title Constructor of objects of class \linkS4class{StQList}
#'
#' @description This constructor returns an object of class \linkS4class{StQList}.
#' The input parameter is a named \code{list} of objects of class \linkS4class{StQ}. Notice that 
#' the names of the \code{list} must be valid time period names according to package 
#' \link[=RepoTime]{RepoTime}.
#'
#' @param Data \code{List} of objects of class \linkS4class{StQ}.
#'
#' @return An object of class \linkS4class{StQList}.
#'
#' @examples
#' EmptyQ <- new(Class = 'StQ')
#' EmptyQList <- vector('list', 12)
#' EmptyQList <- lapply(EmptyQList, function(x) EmptyQ)
#' mm <- c(paste0('0', 1:9), 10:12)
#' names(EmptyQList) <- paste0('MM', mm, '2015')
#' QList <- BuildStQList(EmptyQList)
#' QList
#' #Notice that it is indeed an object with complex structure:
#' str(QList)
#'
#' @include StQ-class.R StQList-class.R
#'
#' @import RepoTime methods
#'
#' @export
BuildStQList <- function(Data){

    if (is.null(names(Data))) stop('[StQ::BuildStQList] Data must be a named list of StQ objects.\n')

    PeriodList <- newRepoTime(names(Data))

    out <- new(Class = 'StQList', Data = Data, Periods = PeriodList)
    validObject(out)

    return(out)
}
