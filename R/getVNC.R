#' @title Return slot \code{VNC} from an object
#'
#' @description \code{getVNC} extracts slot \code{VarNameCorresp} from an object of class 
#' \linkS4class{DD} or class \linkS4class{StQ}.
#'
#' This method returns an object of class \linkS4class{VarNameCorresp} from an object of class 
#' \linkS4class{DD} or \linkS4class{StQ} specified as input argument.
#'
#' @param object Object of class \linkS4class{DD} or \linkS4class{StQ}.
#'
#' @return Object of class \linkS4class{VarNameCorresp} of the input object.
#'
#' @examples 
#' library(data.table)
#' VarList <- list(
#'   ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                   NonIDQual = rep('', 5),
#'                   IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                   NumIdEst = c('', rep('.', 4)),
#'                   UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                   InFiles = rep('FI', 5)),
#'   MicroData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                          NonIDQual = c('', 'Market', ''),
#'                          IDDD = c(rep('', 2), 'NewOrders'),
#'                          NumIdEst = c(rep('', 2), '.'),
#'                          Market = c(rep('', 2), '1.'),
#'                          UnitName = c('numidest', '', 'cp09'),
#'                          InFiles = rep('FF, FD, FG', 3)),
#'  Aggregates_Total = data.table(IDQual = c('Province', 'NACE', 'Market', ''),
#'                                NonIDQual = rep('', 4),
#'                                IDDD = c('', '', '', 'TotalTurnover'),
#'                                Province = c('', '', '', '.'),
#'                                NACE = c('', '', '', '.'),
#'                                Market = c('', '', '', '1.'),
#'                                UnitName = c('provincia', 'actividad', '', 'cn01'),
#'                                InFiles = rep('FP', 4)))
#'                      
#' VNC <- BuildVNC(VarList)
#' getVNC(VNC)
#' 
#' @import data.table
#' 
#' @include DD.R VNC.R BuildVNC.R BuildDD.R getDD.R
#'  
#' @export
setGeneric("getVNC", function(object) {standardGeneric("getVNC")})

#' @rdname getVNC
#'
#' @export
setMethod(
    f = "getVNC",
    signature = c("DD"),
    function(object){object[['VNC']]}
)
#' @rdname getVNC
#'
#' @export
setMethod(
    f = "getVNC",
    signature = c("StQ"),
    function(object){
        
        VNC <- getVNC(getDD(object))
        return(VNC)
        
    }
)
