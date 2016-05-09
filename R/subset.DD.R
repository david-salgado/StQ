#' @title Extract parts of an object of class \linkS4class{DD} 
#' 
#' @description \code{[} extracts parts of an object of class \linkS4class{DD}. 
#' 
#' It is indeed the method \code{[} for the class \linkS4class{DD}. This method 
#' returns subsets from an object of class \linkS4class{DD} specified as an 
#' input parameter. The output is an object of the same class \linkS4class{DD}
#' as the input parameter \code{x}. 
#' 
#' @param x Object of class \linkS4class{DD}. 
#'
#' @param i,j,... indices corresponding to elements to be extracted. The 
#' indices are numeric or character vectors, \code{\link{missing}} or 
#' \code{\link{NULL}}. Numeric values are coerced to \code{integer} with 
#' \code{\link{as.integer}} (thus truncated to zero). 
#'       
#' @param drop Included by coherence. 
#'
#' @return Object of class \linkS4class{DD} with the subsetted input object.
#'  
#' @examples
#' data(ExampleDD)
#' ExampleDD[Variable == 'Turnover']
#' 
#' @include StQ-class.R getData.R setData.R getVNC.R BuildVNC.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
  f = "[",
  signature = c("DD"),
  function(x, i, j, ..., drop = TRUE){
    
    DDslotNames <- setdiff(slotNames(x), 'VarNameCorresp')
    output <- vector("list", length(DDslotNames))
    mc <- match.call()
    for (DDslot in DDslotNames){
        output[[DDslot]] <- mc
        output[[DDslot]][['x']] <- slot(x, DDslot)
        output[[DDslot]] <- eval(output[[DDslot]], envir = parent.frame())
    }
    VNC <- getVNC(x)

    output <- new(Class = 'DD',
                  VarNameCorresp = VNC,
                  ID = if(is.null(output[['ID']])){
                            new(Class = 'DDdt')
                       } else {
                            new(Class = 'DDdt', output[['ID']])},
                  MicroData = if(is.null(output[['MicroData']])){
                            new(Class = 'DDdt')
                       } else {
                           new(Class = 'DDdt', output[['MicroData']])},
                  ParaData = if(is.null(output[['ParaData']])){
                      new(Class = 'DDdt')
                       } else {
                           new(Class = 'DDdt', output[['ParaData']])},
                  Aggregates = if(is.null(output[['Aggregates']])){
                           new(Class = 'DDdt')
                        } else {
                            new(Class = 'DDdt', output[['Aggregates']])},
                  AggWeights = if(is.null(output[['AggWeights']])){
                           new(Class = 'DDdt')
                         } else {
                             new(Class = 'DDdt', output[['AggWeights']])},
                  Other = if(is.null(output[['Other']])){
                           new(Class = 'DDdt')
                         } else {
                             new(Class = 'DDdt', output[['Other']])})
    return(output)
    
  }
)
