#' @title Return a vector with the names of the time intervals.
#'
#' @description \code{getPeriods} returns a vector with the names of the time periods from the slot
#' \code{Periods} of the input \linkS4class{rawStQList} object.
#'
#' @param object Object of class \linkS4class{rawStQList}.
#'
#' @return Character vector with the names of the time periods according to the notation of package
#' \linkS4class{RepoTimeInt}.
#'
#' @examples
#' library(RepoTime)
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' PeriodList <- newRepoTime(TimePer)
#' EmptyrawQ <- new(Class = 'rawStQ')
#' EmptyrawQList <- vector('list', 12)
#' EmptyrawQList <- lapply(EmptyrawQList, function(x) EmptyrawQ)
#' rawQList <- new(Class = 'rawStQList', Data = EmptyrawQList, Periods = PeriodList)
#' getPeriods.rawStQList(rawQList)
#'
#' @export
setGeneric("getPeriods.rawStQList",
           function(object){standardGeneric("getPeriods.rawStQList")})

#' @rdname getPeriods.rawStQList
#'
#'
#' @include rawStQList-class.R
#'
#' @export
setMethod(
    f = "getPeriods.rawStQList",
    signature = c("rawStQList"),
    function(object){
        
        periods <- object@Periods
        out <- periods@Repo
        return(out)
        
    }
)
