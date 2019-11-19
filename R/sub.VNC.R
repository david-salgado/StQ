#' @title Extract parts of an object of class \link{VNC}
#'
#' @description \code{[} extracts parts of an object of class \link{VNC}.
#'
#' It is indeed the method \code{[} for the class \link{VNC}. 
#'
#' @param x Object of class \link{VNC}.
#'
#' @param i,j,... Indices corresponding to elements to be extracted. The indices are numeric or
#' character vectors, \code{\link{missing}} or \code{\link{NULL}}. Numeric values are coerced to
#' \code{integer} with \code{\link{as.integer}} (thus truncated to zero).
#'
#' @param drop Included by coherence.
#'
#' @return Object of class \link{VNC} with the subsetted input object.
#'
#' @examples
#' \dontrun{
#' # Falla porque falta un on= en algún join de data.table
#' # Ademas fallaria al llegar al BuildVNC de esta misma función
#' library(data.table)
#' VNC <- VNC(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                            NonIDQual = c('','','','',''),
#'                            IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                            NumIdEst = c('', rep('.', 4)),
#'                            UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                            InFiles = rep('FI', 5)),
#'            MicroData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                   NonIDQual = c('', 'Market', ''),
#'                                   IDDD = c(rep('', 2), 'NewOrders'),
#'                                   NumIdEst = c(rep('', 2), '.'),
#'                                   Market = c(rep('', 2), '1'),
#'                                   UnitName = c('numidest', '', 'cp09'),
#'                                   InFiles = rep('FF, FD, FG', 3)),
#'            ParaData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                  NonIDQual = c('', 'Action', ''),
#'                                  IDDD = c(rep('', 2), 'Date'),
#'                                  NumIdEst = c(rep('', 2), '.'),
#'                                  Action = c(rep('', 2), 'Imputation'),
#'                                  UnitName = c('numidest', '', 'FechaImput'),
#'                                  InFiles = rep('FP', 3)))
#' VNC[c('ID', 'MicroData')]
#' 
#' }
#'
#' @include VNC.R BuildVNC.R
#'
#' @import data.table
#'
#' @export
`[.VNC` <- function(x, i, j, by, keyby, with=TRUE, nomatch=getOption("datatable.nomatch"), mult="all", roll=FALSE, rollends=if (roll=="nearest") c(TRUE,TRUE) else if (roll>=0) c(FALSE,TRUE) else c(TRUE,FALSE), which=FALSE, .SDcols, verbose=getOption("datatable.verbose"), allow.cartesian=getOption("datatable.allow.cartesian"), drop=NULL, on=NULL){
        
    DDslotNames <- names(x)
    output <- vector("list", length(DDslotNames))
    mc <- match.call()
    output <- lapply(DDslotNames, function(DDslot){
        
        LocalSlot <- x[[DDslot]]
        Localmc <- mc
        Localmc[[1L]] <- data.table:::`[.data.table`
        Localmc[['x']] <- LocalSlot
        LocalOutput <- eval(Localmc)
        if (dim(LocalOutput)[1] == 0) { 
            
            return(NULL)
            
        } else {
            
            LocalSlot <- LocalSlot[IDQual != '' | NonIDQual != '']
            LocalOutput <- rbindlist(list(LocalSlot, LocalOutput))
            return(LocalOutput)
        }
        
    })
    names(output) <- DDslotNames
    output <- output[!sapply(output, is.null)]
    subVNC <- BuildVNC(output)
    return(subVNC)
    
       #Data <- NextMethod()
       #output <- BuildVNC(Data)
       #return(output)
}
