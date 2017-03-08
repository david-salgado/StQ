#' @title S4 class to implement a named list of objects of class \linkS4class{StQ}
#'
#' @description Definition of an S4 class of name \code{StQList} with a list of objects of class
#' \linkS4class{StQ}. Instead of an atomic vector as a name, we associate with it an object of class
#' \linkS4class{RepoTimeInt} containing the names of the time periods corresponding to each
#' \linkS4class{StQ} object.
#'
#' The structure of this S4 class \code{StQList} comprises 2 attributes:
#'
#' \itemize{
#'
#' \item the attribute \code{Data}, which is a \code{list} of objects of class \linkS4class{StQ};
#'
#' \item  the attribute \code{Periods}, which is an object of class \linkS4class{RepoTimeInt}.
#' }
#'
#' @slot Data \code{List} of objects of class \linkS4class{StQ};
#'
#' @slot Periods An object of class \linkS4class{RepoTimeInt};
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
#' QList
#' str(QList)
#'
#' @include StQ-class.R
#' 
#' @import RepoTime
#' 
#' @export
setClass(Class = "StQList",
         slots = c(Data = 'list',
                   Periods ='RepoTimeInt'),
         validity = function(object){
             
             DataClass <- unique(unlist(lapply(object@Data, class)))
             if (!identical(DataClass, 'StQ')) {
                 
                 stop('[StQList::validity] Every component of slot Data must be of class StQ.')
             }
             
             PeriodClass <- object@Periods
             if (length(object@Data) != length(PeriodClass@Repo)) {
                 
                 stop('[StQList::validity] The lenght of both slots must be equal.')
             }
             return(TRUE)
         }
)

