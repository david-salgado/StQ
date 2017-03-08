#' @title Set value of slot \linkS4class{VarNameCorresp}
#'
#' @description \code{setVNC} assigns a  \linkS4class{VarNameCorresp} object to the slot 
#' \code{VarNameCorresp} of the input object.
#'
#' @param object Object containing slot \code{VarNameCorresp} to be set.
#'
<<<<<<< HEAD
#' @param value Object of class \linkS4class{VarNameCorresp}.
=======
#' @param value List of \linkS4class{data.table}s to be assigned to the slot 
#' \linkS4class{VarNameCorresp}.
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @return Object with slot \linkS4class{VarNameCorresp} updated.
#'
#' @examples
#' # On an object of class DD:
#' library(data.table)
#' MicroDataDD <- data.table(Variable = 'IEPEntradaPed', Sort = 'IDDD', Class = 'numeric',
#'                           Length = '8',
#'                           Qual1 = 'NumIdEst', Qual2 = 'Market', ValueRegExp = '')
<<<<<<< HEAD
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
=======
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
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
#' 
#' @rdname setVNC
#'
#' @import data.table
#'
#' @export
setGeneric("setVNC<-", function(object, value){standardGeneric("setVNC<-")})

#' @rdname setVNC
#'
<<<<<<< HEAD
#' @include DD.R VNC.R StQ.R
=======
#' @include DD-class.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setVNC",
<<<<<<< HEAD
    signature = c("DD", "VNC"),
    function(object, value){
        
        object[['VNC']] <- value
=======
    signature = c("DD", "VarNameCorresp"),
    function(object, value){
        
        object@VarNameCorresp <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
<<<<<<< HEAD
    signature = c("StQ", "VNC"),
=======
    signature = c("StQ", "VarNameCorresp"),
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
    function(object, value){
        
        setVNC(object[['DD']]) <- value
        return(object)
    }
)
