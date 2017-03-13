#' @title Show an object of class \linkS4class{StQList}
#'
#' @description \code{show} displays the slot \code{Data} of the input \linkS4class{StQList} object
#' with the names of time periods from the slot \code{Periods}.
#'
#' It is indeed the method \link[methods]{show} adapted to the class \linkS4class{StQList}.
#'
#' @param object Object of class \linkS4class{StQList}.
#'
#' @return Object of class \link{NULL}.
#'
#' @examples
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' PeriodList <- RepoTime::newRepoTime(TimePer)
#' EmptyQ <- StQ()
#' EmptyQList <- vector('list', 12)
#' EmptyQList <- lapply(EmptyQList, function(x) EmptyQ)
#' QList <- StQList(Data = EmptyQList, Periods = PeriodList)
#' show(QList)
#' str(QList)
#'
#' @include StQList.R getPeriods.R
#'
#' @import data.table
#' 
#'
#' @export
setMethod(
  f = "show",
  signature = c("StQList"),
  function(object){

    OutList <- object$Data
    OutList <- lapply(OutList, function(x) x[['Data']])
    names(OutList) <- getPeriods(object)

    show(OutList)

    invisible(NULL)
  }
)
print.StQList <- function(object){show(object)}
