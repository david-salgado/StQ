#' @title Extract parts of an object of class \linkS4class{rawStQList}
#'
#' @description \code{[} extracts parts of an object of class \linkS4class{rawStQList}.
#'
#' It is indeed the method \code{[} for the class \linkS4class{rawStQList}. This method returns a
#' subset of an input object of class \linkS4class{rawStQList}. Thus it returns an object of class
#' \linkS4class{rawStQList}.
#'
#' @param x object of class \linkS4class{rawStQList}.
#'
#' @param i,j,... Indices corresponding to the elements to be extracted. These indices are numeric
#' vector or character vector or \link{missing} or \link{NULL}. Numeric values are coerced
#' internally to \code{integer} through \code{\link{as.integer}} (and thus truncated to zero).
#' Character vector correspond to names of the respective time period of each component object of
#' class \link{StQ}.
#'
#' @param drop Included by coherence.
#'
#' @return Object of class \linkS4class{rawStQList} with the corresponding data subset.
#'
#' @examples
#' \dontrun{
#' # Error in eval(mc, rawStQObj, parent.frame()) : 
#' # invalid 'envir' argument of type 'character' 
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' QList <- vector('list', 12)
#' QList <- lapply(QList, function(x) ExamplerawStQ)
#' names(QList) <- TimePer
#' QList <- rawStQList(Data = QList, Periods = RepoTime::newRepoTime(TimePer))
#' QList[c('MM092015', 'MM102015')]
#' }
#'
#' @include rawStQList.R getData.R
#'
#' @import data.table
#'
#' @export
`[.rawStQList` <- function(x, i, j, by, keyby, with=TRUE, nomatch=getOption("datatable.nomatch"), mult="all", roll=FALSE, rollends=if (roll=="nearest") c(TRUE,TRUE) else if (roll>=0) c(FALSE,TRUE) else c(TRUE,FALSE), which=FALSE, .SDcols, verbose=getOption("datatable.verbose"), allow.cartesian=getOption("datatable.allow.cartesian"), drop=NULL, on=NULL){
    
    mc <- match.call()
    DataList <- getData(x)
    output <- lapply(DataList, function(rawStQObj){
        
        Localmc <- mc
        mc[['x']] <- rawStQObj
        LocalOutput <- eval(mc, rawStQObj, parent.frame())
        return(LocalOutput)
        
    })
    Periods <- names(DataList)
    
    output <- rawStQList(Data = output, Periods = RepoTime::newRepoTime(Periods))
    return(output)    
    
}

