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
<<<<<<< HEAD
#' EmptyrawQ <- rawStQ())
#' EmptyrawQList <- vector('list', 12)
#' EmptyrawQList <- lapply(EmptyrawQList, function(x) EmptyrawQ)
#' rawQList <- rawStQList(Data = EmptyrawQList, Periods = PeriodList)
=======
#' EmptyrawQ <- new(Class = 'rawStQ')
#' EmptyrawQList <- vector('list', 12)
#' EmptyrawQList <- lapply(EmptyrawQList, function(x) EmptyrawQ)
#' rawQList <- new(Class = 'rawStQList', Data = EmptyrawQList, Periods = PeriodList)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' getPeriods(rawQList)
#' 
#' EmptyQ <- new(Class = 'StQ')
#' EmptyQList <- vector('list', 12)
#' EmptyQList <- lapply(EmptyQList, function(x) EmptyQ)
<<<<<<< HEAD
#' QList <- StQList(Data = EmptyQList, Periods = PeriodList)
#' getPeriods(QList)
#' 
#' @include rawStQList.R StQList.R
#' 
=======
#' QList <- new(Class = 'StQList', Data = EmptyQList, Periods = PeriodList)
#' getPeriods(QList)

>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @export
setGeneric("getPeriods", function(object){standardGeneric("getPeriods")})

#' @rdname getPeriods
#'
<<<<<<< HEAD
=======
#' @include rawStQList-class.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @export
setMethod(
    f = "getPeriods",
    signature = c("rawStQList"),
    function(object){
        
<<<<<<< HEAD
        periods <- object$Periods
        output <- RepoTime::getRepo(periods)
=======
        periods <- object@Periods
        output <- periods@Repo
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(output)
    }
)

#' @rdname getPeriods
#'
<<<<<<< HEAD
=======
#' @include StQList-class.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @export
setMethod(
    f = "getPeriods",
    signature = c("StQList"),
    function(object){
        
<<<<<<< HEAD
        periods <- object$Periods
        output <- RepoTime::getRepo(periods)
=======
        periods <- object@Periods
        output <- periods@Repo
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(output)
    }
)
