#' @title Set value of slot \code{Aggregates}
#'
#' @description \code{setAggregates} assigns a \linkS4class{data.table} to the slot \code{Aggregates} of 
#' the input object.
#'
#' @param object Object whose slot \code{Aggregates} is to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{Aggregates}.
#'
#' @return Object with slot Aggregates updated.
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
#' DD <- BuildDD(list(VNC = VNC, MicroData = MicroDataDD))
#'           
#' Aggdt <- data.table(Variable = c('NACE09', 'Turnover'),
#'                     Sort = c('IDQual', 'IDDD'),
#'                     Class = c('character', 'character'),
#'                     Length = c('4', '10'),
#'                     Qual1 = c('', 'NACE09'),
#'                     ValueRegExp = c('([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )'))  
#' setAggregates(DD) <- Aggdt
#' DD
#'
#' @rdname setAggregates
#'
#' @import data.table
#' 
#' @include VNC.R DD.R getVNC.R setVNC.R StQ.R getAggregates.R DDdtToVNC.R plus.VNC.R
#'
#' @export
setGeneric("setAggregates<-", function(object, value){standardGeneric("setAggregates<-")})

#' @rdname setAggregates
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setAggregates",
    signature = c("VNC", "data.table"),
    function(object, value){
        
        VNCValue <- try(BuildVNC(value))
        if (inherits(VNCValue, "try-error")) stop('[StQ::setAggregates] Value does not have the correct format.\n')
        object[['Aggregates']] <- value
        return(object)
    }
)
#' @rdname setAggregates
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setAggregates",
    signature = c("DD", "data.table"),
    function(object, value){
        
        newVNC <- DDdtToVNC(value, NameVNC = 'Aggregates', InFiles = 'FA')
        DDValue <- try(BuildDD(list(VNC = newVNC, Aggregates = value)))
        if (inherits(DDValue, "try-error")) stop('[StQ::setAggregates] Value does not have the correct format.\n')
        object[['VNC']] <- object[['VNC']] + newVNC
        object[['Aggregates']] <- value
        return(object)
    }
)
