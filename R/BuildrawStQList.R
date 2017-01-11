#' @title Constructor of objects of class \linkS4class{rawStQList}
#'
#' @description This constructor returns an object of class \linkS4class{rawStQList}.
#' The input parameter is a named \code{list} of objects of class \linkS4class{rawStQ}. Notice that 
#' the names of the \code{list} must be valid time period names according to package 
#' \link[=RepoTime]{RepoTime}.
#'
#' @param Data A named \code{list} of objects of class \linkS4class{rawStQ}.
#'
#' @return An object of class \linkS4class{rawStQList}.
#'
#' @examples
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' EmptyrawQ <- new(Class = 'rawStQ')
#' EmptyrawQList <- vector('list', 12)
#' EmptyrawQList <- lapply(EmptyrawQList, function(x) EmptyrawQ)
#' names(EmptyrawQList) <- TimePer
#' rawQList <- BuildrawStQList(EmptyrawQList)
#' rawQList
#' #Notice that it is indeed an object with complex structure:
#' str(rawQList)
#'
#' @include rawStQ-class.R rawStQList-class.R
#'
#' @import RepoTime
#'
#' @export
BuildrawStQList <- function(Data){
    
    if (is.null(names(Data))) stop('[StQ::BuildrawStQList] The input parameter Data must be a named list of objects of class rawStQ.\n')
    
    PeriodList <- newRepoTime(names(Data))
    output <- new(Class = 'rawStQList', Data = Data, Periods = PeriodList)
    validObject(output)
    
    return(output)
}
