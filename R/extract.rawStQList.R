#' @title Extract parts of an object of class \linkS4class{rawStQList}
#'
#' @description \code{[[} extracts parts of an object of class \linkS4class{rawStQList}.
#'
#' It is indeed the method \code{[[} for the class \linkS4class{rawStQList}. This method returns
#' objects of class \linkS4class{StQ} from the slot \code{Data} of the input object \code{x}.
#'
#' @param x object of class \linkS4class{rawStQList}.
#'
#' @param i,j,... Indices corresponding to the elements to be extracted. These indices are numeric
#' vector or character vector or \link{missing} or \link{NULL}. Numeric values are coerced
#' internally to \code{integer} through \code{\link{as.integer}} (and thus truncated to zero).
#' Character vector correspond to names of the respective time period of each component object of
#' class \linkS4class{StQ}.
#'
#' @param exact Included by coherence.
#'
#' @return Object of class \linkS4class{StQ}.
#'
#' @examples
#' library(RepoTime)
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' PeriodList <- newRepoTime(TimePer)
#' EmptyQ <- new(Class = 'rawStQ')
#' EmptyQList <- vector('list', 12)
#' EmptyQList <- lapply(EmptyQList, function(x) EmptyQ)
#' QList <- new(Class = 'rawStQList', Data = EmptyQList, Periods = PeriodList)
#' QList[['MM092015']]
#' QList[[2]]
#'
<<<<<<< HEAD
#' @include rawStQList.R getPeriods.R
=======
#' @include rawStQList-class.R getPeriods.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table
#' 
#' @export
<<<<<<< HEAD
`[[.rawStQList` <- function(x, i, j, ..., exact = TRUE){
    
    
    mc <- match.call()
    New.x <- x$Data
    Periods <- getPeriods(x)
    names(New.x) <- Periods
    mc[[1L]] <- `[[`
    mc[['x']] <- New.x
    output <- eval(mc)
    return(output)
    
}

=======
setMethod(
    f = "[[",
    signature = c("rawStQList"),
    function(x, i, j, ..., exact = TRUE){
        
        Data <- x@Data
        names(Data) <- getPeriods(x)
        output <- Data[[i]]
        return(output)
        
    }
)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8