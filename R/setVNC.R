#' @title Set value of slot \code{VarNameCorresp}
#'
#' @description \code{setVNC} assigns a list of \linkS4class{data.table}s to the
#'  slot \code{VarNameCorresp} of the input object.
#'
#' @param object Object containing slot \code{VarNameCorresp} to be assigned.
#'
#' @param value List of \linkS4class{data.table}s to be assigned to the slot 
#' \code{VarNameCorresp}.
#'
#' @return Object with slot \code{VarNameCorresp} updated.
#'
#' @examples
#' # On an object of class DD:
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
#'                                                           UnitName = c('', '', '', 'cp02'),
#'                                                           InFiles = rep('FF', 4))))
#' VNC <- BuildVNC(VarList)
#' DD <- new(Class = 'DD', VarNameCorresp = VNC, MicroData = MicroDataDD)
#' 
#' VarListAdd <- list(MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst','','',''),
#'                                                           NonIDQual = c('','Market','Cod', ''),
#'                                                           IDDD = c('', '', '' , 'IEPEntradaPed'),
#'                                                           NumIdEst = c('', '', '', '.'),
#'                                                           Market = c('', '', '', '2.'),
#'                                                           Cod = rep('', 4),
#'                                                           UnitName = c('', '', '', 'cp02'),
#'                                                           InFiles = rep('FF', 4))))
#' VNCAdd <- BuildVNC(VarListAdd)
#' setVNC(DD) <- VNCAdd
#' DD
#'
#' # On an object of class StQ:
#' library(data.table)
#' MicroDataDD <- data.table(Variable = 'IEPEntradaPed', Sort = 'IDDD', Class = 'numeric',
#'                           Length = '8', Qual1 = 'NOrden', ValueRegExp = '')
#' MicroDataDD <- new(Class = 'DDdt', MicroDataDD)
#' VarList <- list(MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst','','',''),
#'                                                           NonIDQual = c('', 'Market', 'Cod', ''),
#'                                                           IDDD = c('', '', '' ,'IEPEntradaPed'),
#'                                                           NumIdEst = c('', '', '', '.'),
#'                                                           Market = c('', '', '', '1.'),
#'                                                           Cod = rep('', 4),
#'                                                           Unit1 = c('', '', '', 'cp02'))))
#' VNC <- BuildVNC(VarList)
#' DD <- new(Class = 'DD', VarNameCorresp = VNC, MicroData = MicroDataDD)  
#' StQ <- new(Class = 'StQ', Data = new(Class = 'Datadt',
#'                                      data.table(IDDD = character(0), Value = character(0))),
#'                           DD = DD)
#'              
#' VarListAdd <- list(MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst','','',''),
#'                                                           NonIDQual = c('','Market','Cod', ''),
#'                                                           IDDD = c('', '', '' , 'IEPEntradaPed'),
#'                                                           NumIdEst = c('', '', '', '.'),
#'                                                           Market = c('', '', '', '2.'),
#'                                                           Cod = rep('', 4),
#'                                                           Unit1 = c('', '', '', 'cp02'))))
#' VNCAdd <- BuildVNC(VarListAdd)                                                    
#' setVNC(StQ) <- VNCAdd
#' getDD(StQ)
#' 
#' @rdname setVNC
#'
#' @import data.table
#'
#' @export
setGeneric("setVNC<-", function(object, value){standardGeneric("setVNC<-")})

#' @rdname setVNC
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setVNC",
    signature = c("DD", "VarNameCorresp"),
    function(object, value){
        
        object@VarNameCorresp <- value
        validObject(object)
        return(object)
    }
)
#' @rdname setVNC
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setVNC",
    signature = c("StQ", "VarNameCorresp"),
    function(object, value){
        
        setVNC(object@DD) <- value
        validObject(object)
        return(object)
    }
)
