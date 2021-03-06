#' @title Set value of slot \linkS4class{VarNameCorresp}
#'
#' @description \code{setVNC} assigns a  \linkS4class{VarNameCorresp} object to the slot 
#' \code{VarNameCorresp} of the input object.
#'
#' @param object Object containing slot \code{VarNameCorresp} to be set.
#'
#' @param value Object of class \linkS4class{VarNameCorresp}.
#'
#' @return Object with slot \linkS4class{VarNameCorresp} updated.
#'
#' @examples
#' # On an object of class DD:
#' library(data.table)
#' MicroDataDD <- data.table(Variable = 'IEPEntradaPed', Sort = 'IDDD', Class = 'numeric',
#'                           Length = '8',
#'                           Qual1 = 'NumIdEst', Qual2 = 'Market', ValueRegExp = '')
#' VarList <- list(MicroData = data.table(IDQual = c('NumIdEst','','',''),
#'                                        NonIDQual = c('', 'Market', 'Cod', ''),
#'                                        IDDD = c('', '', '' ,'IEPEntradaPed'),
#'                                        NumIdEst = c('', '', '', '.'),
#'                                        Market = c('', '', '', '1.'),
#'                                        Cod = rep('', 4),
#'                                        UnitName = c('', '', '', 'cp02'),
#'                                        InFiles = rep('FF', 4)))
#' VNC <- BuildVNC(VarList)
#' DD <- DD(list(VNC = VNC, MicroData = MicroDataDD))
#' 
#' VarListAdd <- list(MicroData = data.table(IDQual = c('NumIdEst','','',''),
#'                                           NonIDQual = c('','Market','Cod', ''),
#'                                           IDDD = c('', '', '' , 'IEPEntradaPed'),
#'                                           NumIdEst = c('', '', '', '.'),
#'                                           Market = c('', '', '', '2.'),
#'                                           Cod = rep('', 4),
#'                                           UnitName = c('', '', '', 'cp02'),
#'                                           InFiles = rep('FF', 4)))
#' VNCAdd <- BuildVNC(VarListAdd)
#' setVNC(DD) <- VNCAdd
#' DD
#'
#'\dontrun{
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
#'                                                           UnitName = c('', '', '', 'cp02'),
#'                                                           InFiles = rep('FF', 4))))
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
#'                                                           UnitName = c('', '', '', 'cp02'),
#'                                                           InFiles = rep('FF', 4))))
#' VNCAdd <- BuildVNC(VarListAdd)                                                    
#' setVNC(StQ) <- VNCAdd
#' getDD(StQ)
#' }
#' 
#' @rdname setVNC
#'
#' @import data.table
#'
#' @export
setGeneric("setVNC<-", function(object, value){standardGeneric("setVNC<-")})

#' @rdname setVNC
#'
#' @include DD.R VNC.R StQ.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setVNC",
    signature = c("DD", "VNC"),
    function(object, value){
        
        object[['VNC']] <- value
        return(object)
    }
)
#' @rdname setVNC
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setVNC",
    signature = c("StQ", "VNC"),
    function(object, value){
        
        setVNC(object[['DD']]) <- value
        return(object)
    }
)
