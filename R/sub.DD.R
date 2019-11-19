#' @title Extract parts of an object of class \link{DD} 
#' 
#' @description \code{[} extracts parts of an object of class \link{DD}. 
#' 
#' It is indeed the method \code{[} for the class \link{DD}. This method returns subsets from
#' an object of class \link{DD} specified as an input parameter. The output is an object of
#' the same class \link{DD} as the input parameter \code{x}. 
#' 
#' @param x Object of class \link{DD}. 
#'
#' @param i,j,... indices corresponding to elements to be extracted. The indices are numeric or
#' character vectors, \code{\link{missing}} or \code{\link{NULL}}. Numeric values are coerced to
#' \code{integer} with \code{\link{as.integer}} (thus truncated to zero). 
#'       
#' @param drop Included by coherence. 
#'
#' @return Object of class \link{DD} with the subsetted input object.
#'  
#' @examples
#' data(ExampleDD)
#' \dontrun{
#' ExampleDD[Variable == 'Turnover']
#' }
#' 
#' @include getVNC.R BuildDD.R VNC.R DD.R
#' 
#' @import data.table
#' 
#' @export
`[.DD` <- function(x, i, j, by, keyby, with=TRUE, nomatch=getOption("datatable.nomatch"), mult="all", roll=FALSE, rollends=if (roll=="nearest") c(TRUE,TRUE) else if (roll>=0) c(FALSE,TRUE) else c(TRUE,FALSE), which=FALSE, .SDcols, verbose=getOption("datatable.verbose"), allow.cartesian=getOption("datatable.allow.cartesian"), drop=NULL, on=NULL){
    
    DDslotNames <- setdiff(names(x), 'VNC')
    output <- vector("list", length(DDslotNames))
    mc <- match.call()
    output <- lapply(DDslotNames, function(DDslot){
        
        LocalSlot <- x[[DDslot]]
        Localmc <- mc
        Localmc[[1L]] <- data.table:::`[.data.table`
        Localmc[['x']] <- LocalSlot
        LocalOutput <- eval(Localmc, x, parent.frame())
        if (dim(LocalOutput)[1] == 0) { 
            
            return(NULL)
            
        } else {
            
            return(LocalOutput)
        }
        
    })
    names(output) <- DDslotNames
    output <- output[!sapply(output, is.null)]
    VNC <- getVNC(x)
    DataDD <- c(list(VNC = VNC), output)
    DD <- BuildDD(DataDD)
    return(DD)
}
