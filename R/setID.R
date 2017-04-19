#' @title Set value of slot \code{ID} of an object \linkS4class{DD}
#'
#' @description \code{setID} assigns a \linkS4class{DDdt} to the slot \code{ID} of the input object.
#'
#' @param object Object containing slot \code{ID} to be assigned.
#'
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{ID}.
#'
#' @return Object \linkS4class{DD} with slot \code{ID} updated.
#'
#' @examples
#' library(data.table)
#' MicroDataDD <- data.table(Variable = 'NewOrders', Sort = 'IDDD', Class = 'numeric',
#'                           Length = '8',
#'                           Qual1 = 'NumIdEst', Qual2 = 'Market', ValueRegExp = '')
#' VarList <- list(MicroData = data.table(IDQual = c('NumIdEst','','',''),
#'                                        NonIDQual = c('', 'Market', 'Cod', ''),
#'                                        IDDD = c('', '', '' ,'NewOrders'),
#'                                        NumIdEst = c('', '', '', '.'),
#'                                        Market = c('', '', '', '1.'),
#'                                        Cod = rep('', 4),
#'                                        UnitName = c('', '', '', 'cp02'),
#'                                        InFiles = rep('FF', 4)))
#' VNC <- BuildVNC(VarList)
#' DD <- DD(VNC = VNC, MicroData = MicroDataDD)
#' 
#' IDdt <- data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                    Sort = c('IDQual', rep('IDDD', 4)),
#'                    Class = rep('character', 5),
#'                    Length = c('11', '20', '20', '20', '9'),
#'                    Qual1 = c('', rep('NumIdEst', 4)),
#'                    ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}'))
#' setID(DD) <- IDdt
#' DD
#' 
#' @rdname setID
#'
#' @import data.table
#'
#' @include VNC.R DD.R getVNC.R DDdtToVNC.R
#'
#' @export
setGeneric("setID<-", function(object, value){standardGeneric("setID<-")})

#' @rdname setID
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setID",
    signature = c("VNC", "data.table"),
    function(object, value){
        
        VNCValue <- try(BuildVNC(value))
        if (inherits(VNCValue, "try-error")) stop('[StQ::setID] Value does not have the correct format.\n')
        object[['ID']] <- value
        return(object)
    }
)
#' @rdname setID
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setID",
    signature = c("DD", "data.table"),
    function(object, value){
        
        newVNC <- DDdtToVNC(value, NameVNC = 'ID', InFiles = 'FI')
        DDValue <- try(BuildDD(list(VNC = newVNC, ID = value)))
        if (inherits(DDValue, "try-error")) stop('[StQ::setID] Value does not have the correct format.\n')
        object[['VNC']] <- object[['VNC']] + newVNC
        object[['ID']] <- value
        return(object)
    }
)
