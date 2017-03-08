#' @title Return a vector with the names of the time period intervals
#'
#' @description \code{getPeriods} returns a vector with the names of the time periods from the input
#'  object.
#'
#' @param object Object of class \linkS4class{rawStQList} or \linkS4class{StQList}.
#'
#' @return Character vector with the names of the time periods according to the notation of package
#' \linkS4class{RepoTimeInt}.
#'
#' @examples
#' library(RepoTime)
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' PeriodList <- newRepoTime(TimePer)
#' 
#' EmptyrawQ <- rawStQ())
#' EmptyrawQList <- vector('list', 12)
#' EmptyrawQList <- lapply(EmptyrawQList, function(x) EmptyrawQ)
#' rawQList <- rawStQList(Data = EmptyrawQList, Periods = PeriodList)
#' getPeriods(rawQList)
#' 
#' EmptyQ <- new(Class = 'StQ')
#' EmptyQList <- vector('list', 12)
#' EmptyQList <- lapply(EmptyQList, function(x) EmptyQ)
#' QList <- StQList(Data = EmptyQList, Periods = PeriodList)
#' getPeriods(QList)
#' 
#' @include rawStQList.R StQList.R
#' 
#'
#' @export
setGeneric("getPeriods", function(object){standardGeneric("getPeriods")})

#' @rdname getPeriods
#'
#' @export
setMethod(
    f = "getPeriods",
    signature = c("rawStQList"),
    function(object){
        
        periods <- object$Periods
        output <- RepoTime::getRepo(periods)
        return(output)
    }
)

#' @rdname getPeriods
#'
#' @export
setMethod(
    f = "getPeriods",
    signature = c("StQList"),
    function(object){
        
        periods <- object$Periods
        output <- RepoTime::getRepo(periods)
        return(output)
    }
)
