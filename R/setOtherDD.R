#' @title Set value of slot \code{Other}
#'
#' @description \code{setOtherDD} assigns a \linkS4class{DDdt} to the slot \code{Other} of the input
#'  object.
#'
#' @param object Object whose slot \code{Other} is to be assigned.
#'
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{Other}.
#'
#' @return Object with slot \code{Other} updated.
#'
#' @examples
#' library(data.table)
#' MicroDataDD <- data.table(Variable = 'NewOrders', Sort = 'IDDD', Class = 'numeric',
#'                           Length = '8',
#'                           Qual1 = 'NumIdEst', Qual2 = 'Market', ValueRegExp = '')
#' MicroDataDD <- new(Class = 'DDdt', MicroDataDD)
#' VarList <- list(MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst','','',''),
#'                                                           NonIDQual = c('', 'Market', 'Cod', ''),
#'                                                           IDDD = c('', '', '' ,'NewOrders'),
#'                                                           NumIdEst = c('', '', '', '.'),
#'                                                           Market = c('', '', '', '1.'),
#'                                                           Cod = rep('', 4),
#'                                                           UnitName = c('', '', '', 'cp02'),
#'                                                           InFiles = rep('FF', 4))))
#' VNC <- BuildVNC(VarList)
#' DD <- new(Class = 'DD', VarNameCorresp = VNC, MicroData = MicroDataDD)
#'           
#' Otherdt <- data.table(Variable = c('NACE08'), Sort = c('NonIDQual'), Class = c('character'),
#'                       Length = '4',
#'                       Qual1 = c(''), ValueRegExp = c('([0-4][0-9])|(5[0-2])'))
#' Otherdt <- new(Class = 'DDdt', Otherdt)             
#'
#' setOtherDD(DD) <- Otherdt
#' DD
#'
#' @rdname setOtherDD
#'
#' @import data.table
#'
#' @export
setGeneric("setOtherDD<-", function(object, value){standardGeneric("setOtherDD<-")})
#' @rdname setOtherDD
#'
#' @include StQ-class.R getVNC.R DDdtToVNC.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setOtherDD",
    signature = c("DD", "DDdt"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))
        
        VNCaux <- getVNC(object)
        VNCaux[['Other']] <- new(Class = 'VNCdt')
        
        setVNC(object) <- DDdtToVNC(value, 'Other') + VNCaux
        object@Other <- value
        validObject(object)
        return(object)
    }
)
#' @rdname setOtherDD
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setOtherDD",
    signature = c("StQ", "DDdt"),
    function(object, value){
        
        setOtherDD(object@DD) <- value
        validObject(object)
        return(object)
    }
)
