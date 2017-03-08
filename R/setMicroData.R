#' @title Set value of slot \code{MicroData} of an object \linkS4class{DD}
#'
#' @description \code{setMicroData} assigns a \linkS4class{DDdt} to the slot \code{ID} of the input 
#' object.
#'
#' @param object Object containing slot \code{ID} to be assigned.
#'
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{ID}.
#'
#' @return Object \linkS4class{DD} with slot \code{ID} updated.
#'
#' @examples
#' library(data.table)
#' MicroDataDD <- data.table(Variable = 'NewOrders', Sort = 'IDDD', Class = 'numeric',
#'                           Length = '8',
#'                           Qual1 = 'NumIdEst', Qual2 = 'Market', ValueRegExp = '')
<<<<<<< HEAD
#' VarList <- list(MicroData = data.table(IDQual = c('NumIdEst','','',''),
#'                                        NonIDQual = c('', 'Market', 'Cod', ''),
#'                                        IDDD = c('', '', '' , 'NewOrders'),
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
#'                                                           IDDD = c('', '', '' , 'NewOrders'),
#'                                                           NumIdEst = c('', '', '', '.'),
#'                                                           Market = c('', '', '', '1.'),
#'                                                           Cod = rep('', 4),
#'                                                           UnitName = c('', '', '', 'cp02'),
#'                                                           InFiles = rep('FF', 4))))
#' VNC <- BuildVNC(VarList)
#' DD <- new(Class = 'DD', VarNameCorresp = VNC, MicroData = MicroDataDD)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' Microdt <- data.table(Variable = c('NumIdEst', 'NewOrders', 'Turnover'),
#'                       Sort = c('IDQual', 'IDDD', 'IDDD'),
#'                       Class = c('character', 'numeric', 'numeric'),
#'                       Length = c('11', '8', '8'),
#'                       Qual1 = c('', 'NumIdEst', 'NumIdEst'),
#'                       ValueRegExp = c('[0-9]{9}PP', '([0-9]{1, 10}| )', '([0-9]{1, 10}| )'))
<<<<<<< HEAD
=======
#' Microdt <- new(Class = 'DDdt', Microdt)
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' setMicroData(DD) <- Microdt
#' DD
#'
#' @rdname setMicroData
#'
#' @import data.table
<<<<<<< HEAD
#' 
#' @include VNC.R DD.R getVNC.R DDdtToVNC.R
=======
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @export
setGeneric("setMicroData<-", function(object, value){standardGeneric("setMicroData<-")})

#' @rdname setMicroData
#'
<<<<<<< HEAD
=======
#' @include DD-class.R getVNC.R DDdtToVNC.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setMicroData",
<<<<<<< HEAD
    signature = c("VNC", "data.table"),
    function(object, value){
        
        VNCValue <- try(BuildVNC(value))
        if (inherits(VNCValue, "try-error")) stop('[StQ::setMicroData] Value does not have the correct format.\n')
        object[['MicroData']] <- value
=======
    signature = c("DD", "DDdt"),
    function(object, value){

        setkeyv(value, setdiff(names(value), 'Value'))

        VNCaux <- getVNC(object)
        VNCaux[['MicroData']] <- new(Class = 'VNCdt')

        setVNC(object) <- DDdtToVNC(value, 'MicroData') + VNCaux
        object@MicroData <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)
#' @rdname setMicroData
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
    f = "setMicroData",
<<<<<<< HEAD
    signature = c("DD", "data.table"),
    function(object, value){
        
        newVNC <- DDdtToVNC(value, NameVNC = 'MicroData', InFiles = 'FF')
        DDValue <- try(BuildDD(list(VNC = newVNC, MicroData = value)))
        if (inherits(DDValue, "try-error")) stop('[StQ::MicroData] Value does not have the correct format.\n')
        object[['VNC']] <- object[['VNC']] + newVNC
        object[['MicroData']] <- value
=======
    signature = c("StQ", "DDdt"),
    function(object, value){

        setMicroData(object@DD) <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)
