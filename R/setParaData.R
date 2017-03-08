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
<<<<<<< HEAD
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
=======
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
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'           
#' Paradt <- data.table(
#'              Variable = c('NumIdEst', 'Date'),
#'              Sort = c('IDQual', 'IDDD'),
#'              Class = rep('character', 2),
#'              Length = c('11', '8'),
#'              Qual1 = c('', 'NumIdEst'),
#'              ValueRegExp = c('[0-9]{9}PP', 
#'                              '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )'))       
<<<<<<< HEAD
=======
#' Paradt <- new(Class = 'DDdt', Paradt)          
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' setParaData(DD) <- Paradt
#' DD
#' 
#' @rdname setParaData
#'
#' @import data.table
#'
<<<<<<< HEAD
#' @include VNC.R DD.R getVNC.R DDdtToVNC.R
#'
#' @export
setGeneric("setParaData<-", function(object, value){standardGeneric("setParaData<-")})

#' @rdname setParaData
#'
=======
#' @export
setGeneric("setParaData<-", function(object, value){standardGeneric("setParaData<-")})
#' @rdname setParaData
#'
#' @include StQ-class.R getVNC.R DDdtToVNC.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setParaData",
<<<<<<< HEAD
    signature = c("VNC", "data.table"),
    function(object, value){
        
        VNCValue <- try(BuildVNC(value))
        if (inherits(VNCValue, "try-error")) stop('[StQ::setParaData] Value does not have the correct format.\n')
        object[['ParaData']] <- value
=======
    signature = c("DD", "DDdt"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))

        VNCaux <- getVNC(object)
        VNCaux[['ParaData']] <- new(Class = 'VNCdt')
        
        setVNC(object) <- DDdtToVNC(value, 'ParaData') + VNCaux
        object@ParaData <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)
#' @rdname setParaData
#'
<<<<<<< HEAD
=======
#' @include StQ-class.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setParaData",
<<<<<<< HEAD
    signature = c("DD", "data.table"),
    function(object, value){
        
        newVNC <- DDdtToVNC(value, NameVNC = 'ParaData', InFiles = 'FA')
        DDValue <- try(BuildDD(list(VNC = newVNC, ParaData = value)))
        if (inherits(DDValue, "try-error")) stop('[StQ::setParaData] Value does not have the correct format.\n')
        object[['VNC']] <- object[['VNC']] + newVNC
        object[['ParaData']] <- value
=======
    signature = c("StQ", "DDdt"),
    function(object, value){
        
        setParaData(object@DD) <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)