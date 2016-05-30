#' @title Constructor of objects of class \linkS4class{StQList}.
#'
#' @description This constructor returns objects of class \linkS4class{StQList}.
#' The input parameter is a \code{list} of objects of class \linkS4class{StQ}.
#'
#' @param Data \code{List} of objects of class \linkS4class{StQ}.
#'
#' @return An object of class \linkS4class{StQList}.
#'
#' @examples
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' EmptyQ <- new(Class = 'StQ')
#' EmptyQList <- vector('list', 12)
#' EmptyQList <- lapply(EmptyQList, function(x) EmptyQ)
#' names(EmptyQList) <- TimePer
#'
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

    if (is.null(names(Data))) stop('[StQ::BuildStQList] Data must be a named list of StQ objects.')

    PeriodList <- newRepoTime(names(Data))

    out <- new(Class = 'StQList', Data = Data, Periods = PeriodList)
    validObject(out)

    return(out)
}
