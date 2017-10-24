#' @title Set value of slot \code{MicroData} of an object \linkS4class{DD}
#'
#' @description \code{setMicroData} assigns a \linkS4class{data.table} to the slot \code{ID} of the  
#' input object.
#'
#' @param object Object containing slot \code{ID} to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{ID}.
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
#'                                        IDDD = c('', '', '' , 'NewOrders'),
#'                                        NumIdEst = c('', '', '', '.'),
#'                                        Market = c('', '', '', '1.'),
#'                                        Cod = rep('', 4),
#'                                        UnitName = c('', '', '', 'cp02'),
#'                                        InFiles = rep('FF', 4)))
#' VNC <- BuildVNC(VarList)
#' DD <- DD(VNC = VNC, MicroData = MicroDataDD)
#'
#' Microdt <- data.table(Variable = c('NumIdEst', 'NewOrders', 'Turnover'),
#'                       Sort = c('IDQual', 'IDDD', 'IDDD'),
#'                       Class = c('character', 'numeric', 'numeric'),
#'                       Length = c('11', '8', '8'),
#'                       Qual1 = c('', 'NumIdEst', 'NumIdEst'),
#'                       ValueRegExp = c('[0-9]{9}PP', '([0-9]{1, 10}| )', '([0-9]{1, 10}| )'))
#' setMicroData(DD) <- Microdt
#' DD
#'
#' @rdname setMicroData
#'
#' @import data.table
#' 
#' @include VNC.R DD.R getVNC.R DDdtToVNC.R
#'
#' @export
setGeneric("setMicroData<-", function(object, value){standardGeneric("setMicroData<-")})

#' @rdname setMicroData
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setMicroData",
    signature = c("VNC", "data.table"),
    function(object, value){
        
        VNCValue <- try(BuildVNC(value))
        if (inherits(VNCValue, "try-error")) stop('[StQ::setMicroData] Value does not have the correct format.\n')
        object[['MicroData']] <- value
        return(object)
    }
)
#' @rdname setMicroData
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setMicroData",
    signature = c("DD", "data.table"),
    function(object, value){
        
        newVNC <- DDdtToVNC(value, NameVNC = 'MicroData', InFiles = 'FF')
        DDValue <- try(BuildDD(list(VNC = newVNC, MicroData = value)))
        if (inherits(DDValue, "try-error")) stop('[StQ::MicroData] Value does not have the correct format.\n')
        object[['VNC']] <- object[['VNC']] + newVNC
        object[['MicroData']] <- value
        return(object)
    }
)
