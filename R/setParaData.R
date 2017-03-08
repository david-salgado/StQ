#' @title Set value of slot \code{ParaData}
#'
#' @description \code{setParaData} assigns a \linkS4class{DDdt} to the slot \code{ParaData} of the 
#' input object.
#'
#' @param object Object whose slot \code{ParaData} is to be assigned.
#'
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{ParaData}.
#'
#' @return Object with slot ParaData updated.
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
#' Paradt <- data.table(
#'              Variable = c('NumIdEst', 'Date'),
#'              Sort = c('IDQual', 'IDDD'),
#'              Class = rep('character', 2),
#'              Length = c('11', '8'),
#'              Qual1 = c('', 'NumIdEst'),
#'              ValueRegExp = c('[0-9]{9}PP', 
#'                              '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )'))       
#' setParaData(DD) <- Paradt
#' DD
#' 
#' @rdname setParaData
#'
#' @import data.table
#'
#' @include VNC.R DD.R getVNC.R DDdtToVNC.R
#'
#' @export
setGeneric("setParaData<-", function(object, value){standardGeneric("setParaData<-")})

#' @rdname setParaData
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setParaData",
    signature = c("VNC", "data.table"),
    function(object, value){
        
        VNCValue <- try(BuildVNC(value))
        if (inherits(VNCValue, "try-error")) stop('[StQ::setParaData] Value does not have the correct format.\n')
        object[['ParaData']] <- value
        return(object)
    }
)
#' @rdname setParaData
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setParaData",
    signature = c("DD", "data.table"),
    function(object, value){
        
        newVNC <- DDdtToVNC(value, NameVNC = 'ParaData', InFiles = 'FA')
        DDValue <- try(BuildDD(list(VNC = newVNC, ParaData = value)))
        if (inherits(DDValue, "try-error")) stop('[StQ::setParaData] Value does not have the correct format.\n')
        object[['VNC']] <- object[['VNC']] + newVNC
        object[['ParaData']] <- value
        return(object)
    }
)
