#' @title Set value of slot \code{MicroData} of an object \linkS4class{DD}
#'
#' @description \code{setMicroData} assigns a \linkS4class{DDdt} to the
#'  slot \code{ID} of the input object.
#'
#' @param object Object containing slot \code{ID} to be assigned.
#'
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{ID}.
#'
#' @return Object \linkS4class{DD} with slot \code{ID} updated.
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
#' Microdt <- data.table(Variable = c('NumIdEst', 'NewOrders'),
#'                       Sort = c('IDQual', 'IDDD'),
#'                       Class = c('character', 'character'),
#'                       Length = c('11', '8'),
#'                       Qual1 = c('', 'NumIdEst'),
#'                       ValueRegExp = c('[0-9]{9}PP', '([0-9]{1, 10}| )'))
#' Microdt <- new(Class = 'DDdt', Microdt)
#'
#' setMicroData(DD) <- Microdt
#' DD
#'
#' @rdname setMicroData
#'
#' @import data.table
#'
#' @export
setGeneric("setMicroData<-", function(object, value){standardGeneric("setMicroData<-")})

#' @rdname setMicroData
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setMicroData",
    signature = c("DD", "DDdt"),
    function(object, value){

        setkeyv(value, setdiff(names(value), 'Value'))

        VNCaux <- getVNC(object)
        VNCaux[['MicroData']] <- new(Class = 'VNCdt')

        setVNC(object) <- DDdtToVNC(value, 'MicroData') + VNCaux
        object@MicroData <- value
        validObject(object)
        return(object)
    }
)
#' @rdname setMicroData
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setMicroData",
    signature = c("StQ", "DDdt"),
    function(object, value){

        setMicroData(object@DD) <- value
        validObject(object)
        return(object)
    }
)
