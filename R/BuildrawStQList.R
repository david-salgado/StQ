#' @title Constructor of objects of class \linkS4class{rawStQList}.
#'
#' @description This constructor returns objects of class \linkS4class{rawStQList}.
#' The input parameter is a \code{list} of objects of class \linkS4class{rawStQ}.
#'
#' @param Data \code{List} of objects of class \linkS4class{rawStQ}.
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
#'
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
    
    if (is.null(names(Data))) stop('[StQ::BuildrawStQList] Data must be a named list of rawStQ objects.')
    
    PeriodList <- newRepoTime(names(Data))
    
    out <- new(Class = 'rawStQList', Data = Data, Periods = PeriodList)
    validObject(out)
    
    return(out)
}
