#' @title Set value of slot \code{Other}
#'
#' @description \code{setOtherDD} assigns a \linkS4class{data.table} to the slot \code{Other} of the
#' input object.
#'
#' @param object Object whose slot \code{Other} is to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{Other}.
#'
#' @return Object with slot \code{Other} updated.
#'
#' @examples
#' \dontrun{
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
#' Otherdt <- data.table(Variable = c('NACE08'), Sort = c('NonIDQual'), Class = c('character'),
#'                       Length = '4',
#'                       Qual1 = c(''), ValueRegExp = c('([0-4][0-9])|(5[0-2])'))
#' setOther(DD) <- Otherdt
#' DD
#' }
#'
#' @rdname setOther
#'
#' @import data.table
#'
#' @include VNC.R DD.R getVNC.R DDdtToVNC.R
#'
#' @export
setGeneric("setOther<-", function(object, value){standardGeneric("setOther<-")})

#' @rdname setOther
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setOther",
    signature = c("VNC", "data.table"),
    function(object, value){
        
        VNCValue <- try(BuildVNC(value))
        if (inherits(VNCValue, "try-error")) stop('[StQ::setOther] Value does not have the correct format.\n')
        object[['Other']] <- value
        return(object)
    }
)
#' @rdname setOther
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setOther",
    signature = c("DD", "data.table"),
    function(object, value){
        
        newVNC <- DDdtToVNC(value, NameVNC = 'Other', InFiles = 'FA')
        DDValue <- try(BuildDD(list(VNC = newVNC, Other = value)))
        if (inherits(DDValue, "try-error")) stop('[StQ::setOther] Value does not have the correct format.\n')
        object[['VNC']] <- object[['VNC']] + newVNC
        object[['Other']] <- value
        return(object)
    }
)
