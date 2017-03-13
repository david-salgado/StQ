#' @title S4 class to implement a named list of objects of class \linkS4class{rawStQ}
#'
#' @description Definition of an S4 class of name \code{rawStQList} with a list of objects of class
#' \linkS4class{rawStQ}. Instead of an atomic vector as a name, we associate with it an object of
#' class \linkS4class{RepoTimeInt} containing the names of the time periods corresponding to each
#' \linkS4class{rawStQ} object.
#'
#' The structure of this S4 class \code{rawStQList} comprises 2 attributes:
#'
#' \itemize{
#'
#' \item the attribute \code{Data}, which is a \code{list} of objects of class \linkS4class{rawStQ};
#'
#' \item  the attribute \code{Periods}, which is an object of class \linkS4class{RepoTimeInt}.
#' }
#'
#' @slot Data \code{List} of objects of class \linkS4class{rawStQ};
#'
#' @slot Periods An object of class \linkS4class{RepoTimeInt};
#'
#' @examples
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' PeriodList <- RepoTime::newRepoTime(TimePer)
#' EmptyrawQ <- rawStQ()
#' EmptyrawQList <- vector('list', 12)
#' EmptyrawQList <- lapply(EmptyrawQList, function(x) EmptyrawQ)
#' rawQList <- rawStQList(Data = EmptyrawQList, Periods = PeriodList)
#' rawQList
#' str(rawQList)
#'
#' @include rawStQ.R
#' 
#' @export
rawStQList <- function(Data = list(), Periods = new(Class = 'RepoTimeInt')){

    object <- list(Data = Data, Periods = Periods)
    PeriodClass <- object$Periods
    if (length(object$Data) != length(RepoTime::getRepo(PeriodClass))) {
        
        stop('[StQList::validity] The lenght of both slots must be equal.')
    }
    if (length(object$Data) != 0){
        
        CorrectClass <- unlist(lapply(object$Data, inherits, 'rawStQ'))
        
        if (!all(CorrectClass)) {
            
            stop('[StQList::validity] Every component of slot Data must be of class rawStQ.')
        }
    }
    
    class(object) <- append("rawStQList", class(object))
    return(object)    
}

setOldClass(c('list'))
setOldClass(c('rawStQList', 'list'))
