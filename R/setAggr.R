#' @title Set value of slot \code{Aggregates}.
#'
#' @description \code{setAggr} assigns a \linkS4class{DDdt} to the slot
#' \code{Aggregates} of the input object.
#'
#' @param object Object whose slot \code{Aggregates} is to be assigned.
#'
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{Aggregates}.
#'
#' @return Object with slot Aggregates updated.
#'
#' @examples
#' # An example:
#' library(data.table)
#' MicroDataDD <- data.table(Variable = 'IEPEntradaPed', Sort = 'IDDD', Class = 'numeric',
#'                           Length = '8',
#'                           Qual1 = 'NumIdEst', Qual2 = 'Market', ValueRegExp = '')
#' MicroDataDD <- new(Class = 'DDdt', MicroDataDD)
#' VarList <- list(MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst','','',''),
#'                                                           NonIDQual = c('', 'Market', 'Cod', ''),
#'                                                           IDDD = c('', '', '' ,'IEPEntradaPed'),
#'                                                           NumIdEst = c('', '', '', '.'),
#'                                                           Market = c('', '', '', '1.'),
#'                                                           Cod = rep('', 4),
#'                                                           UnitName = c('', '', '', 'cp02'))))
#' VNC <- BuildVNC(VarList)
#' DD <- new(Class = 'DD', VarNameCorresp = VNC, MicroData = MicroDataDD)
#'           
#' Aggdt <- data.table(Variable = c('NACE09', 'Turnover'),
#'                     Sort = c('IDQual', 'IDDD'),
#'                     Class = c('character', 'character'),
#'                     Length = c('4', '10'),
#'                     Qual1 = c('', 'NACE09'),
#'                     ValueRegExp = c('([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )'))  
#' Aggdt <- new(Class = 'DDdt', Aggdt)          
#' setAggr(DD) <- Aggdt
#' DD
#'
#' @rdname setAggr
#'
#' @import data.table
#'
#' @export
setGeneric("setAggr<-", function(object, value){standardGeneric("setAggr<-")})
#' @rdname setAggr
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setAggr",
    signature = c("DD", "DDdt"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))

        VNCaux <- getVNC(object)
        VNCaux[['Aggregates']] <- new(Class = 'VNCdt')
        
        setVNC(object) <- DDdtToVNC(value, 'Aggregates') + VNCaux
        object@Aggregates <- value
        validObject(object)
        return(object)
    }
)
#' @rdname setAggr
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setAggr",
    signature = c("StQ", "DDdt"),
    function(object, value){
        
        setAggr(object@DD) <- value
        validObject(object)
        return(object)
    }
)
