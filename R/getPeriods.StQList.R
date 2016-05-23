#' @title Return a vector with the names of the time intervals.
#'
#' @description \code{getPeriods} returns a vector with the names of the time periods from the slot
#' \code{Periods} of the input \linkS4class{StQList} object.
#'
#' @param object Object of class \linkS4class{StQList}.
#'
#' @return Character vector with the names of the time periods according to the notation of package
#' \linkS4class{RepoTimeInt}.
#'
#' @examples
#' library(RepoTime)
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' PeriodList <- newRepoTime(TimePer)
#' EmptyQ <- new(Class = 'StQ')
#' EmptyQList <- vector('list', 12)
#' EmptyQList <- lapply(EmptyQList, function(x) EmptyQ)
#' QList <- new(Class = 'StQList', Data = EmptyQList, Periods = PeriodList)
#' getPeriods.StQList(QList)
#'
#' @export
setGeneric("getPeriods.StQList",
           function(object){standardGeneric("getPeriods.StQList")})

#' @rdname getPeriods.StQList
#'
#'
#' @include StQList-class.R
#'
#' @export
setMethod(
  f = "getPeriods.StQList",
  signature = c("StQList"),
  function(object){

    periods <- object@Periods
    out <- periods@Repo
    return(out)

  }
)
