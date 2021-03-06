#' @title Extract parts of an object of class \linkS4class{StQList}
#'
#' @description \code{[} extracts parts of an object of class \linkS4class{StQList}.
#'
#' It is indeed the method \code{[} for the class \linkS4class{StQList}. This method returns a
#' subset of an input object of class \linkS4class{StQList}. Thus it returns an object of class
#' \linkS4class{StQList}.
#'
#' @param x object of class \linkS4class{StQList}.
#'
#' @param i,j,... Indices corresponding to the elements to be extracted. These indices are numeric
#' vector or character vector, objects of class \linkS4class{RepoTimeInt} or \link{missing} or
#' \link{NULL}. Numeric values are coerced internally to \code{integer} through
#' \code{\link{as.integer}} (and thus truncated to zero). Character vector correspond to names of
#' the respective time period of each component object of class \link{StQ}.
#'
#' @param drop Included by coherence.
#'
#' @return Object of class \linkS4class{StQList} with the corresponding data
#' subset.
#'
#' @examples
#' \dontrun{
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' QList <- vector('list', 12)
#' QList <- lapply(QList, function(x) ExampleStQ)
#' names(QList) <- TimePer
#' StQList <- StQList(Data = QList, Periods = RepoTime::newRepoTime(TimePer))
#' StQList[c('MM092015', 'MM102015')]
#' }
#'
#' @include StQList.R getData.R sub.StQ.R
#'
#' @import data.table
#'
#' @export
setGeneric("subPeriods", function(x, i){standardGeneric("subPeriods")})

#' @rdname subPeriods
#'
#' @export
setMethod(
    f = "subPeriods",
    signature = c("StQList"),
    function(x, i){

     DataList <- getData(x)
     if (class(i) == 'RepoTimeInt') i <- getRepo(i)

     output <- DataList[i]
     names(output) <- names(DataList[i])

     output <- StQList(Data = output, Periods = RepoTime::newRepoTime(names(output)))

     return(output)
    }
)

