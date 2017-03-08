#' @title Set value of slot \code{ID} of an object \linkS4class{DD}
#'
#' @description \code{setID} assigns a \linkS4class{DDdt} to the slot \code{ID} of the input object.
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
#' IDdt <- data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                    Sort = c('IDQual', rep('IDDD', 4)),
#'                    Class = rep('character', 5),
#'                    Length = c('11', '20', '20', '20', '9'),
#'                    Qual1 = c('', rep('NumIdEst', 4)),
#'                    ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}'))
<<<<<<< HEAD
=======
#' IDdt <- new(Class = 'DDdt', IDdt)
#' 
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' setID(DD) <- IDdt
#' DD
#' 
#' @rdname setID
#'
#' @import data.table
#'
<<<<<<< HEAD
#' @include VNC.R DD.R getVNC.R DDdtToVNC.R
#'
=======
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @export
setGeneric("setID<-", function(object, value){standardGeneric("setID<-")})

#' @rdname setID
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
    f = "setID",
<<<<<<< HEAD
    signature = c("VNC", "data.table"),
    function(object, value){
        
        VNCValue <- try(BuildVNC(value))
        if (inherits(VNCValue, "try-error")) stop('[StQ::setID] Value does not have the correct format.\n')
        object[['ID']] <- value
=======
    signature = c("DD", "DDdt"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))

        VNCaux <- getVNC(object)
        VNCaux[['ID']] <- new(Class = 'VNCdt')
        
        setVNC(object) <- DDdtToVNC(value, 'ID') + VNCaux
        object@ID <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)
#' @rdname setID
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
    f = "setID",
<<<<<<< HEAD
    signature = c("DD", "data.table"),
    function(object, value){
        
        newVNC <- DDdtToVNC(value, NameVNC = 'ID', InFiles = 'FI')
        DDValue <- try(BuildDD(list(VNC = newVNC, ID = value)))
        if (inherits(DDValue, "try-error")) stop('[StQ::setID] Value does not have the correct format.\n')
        newVNC <- getVNC(object) + newVNC
        setVNC(object) <- getVNC(object) + newVNC
        object[['ID']] <- value
=======
    signature = c("StQ", "DDdt"),
    function(object, value){
        
        setID(object@DD) <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)
