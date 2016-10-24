#' @title Set value of slot \code{ID} of an object \linkS4class{DD}
#'
#' @description \code{setID} assigns a \linkS4class{DDdt} to the
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
#'                                                           UnitName = c('', '', '', 'cp02'),
#'                                                           InFiles = rep('FF', 4))))
#' VNC <- BuildVNC(VarList)
#' DD <- new(Class = 'DD', VarNameCorresp = VNC, MicroData = MicroDataDD)
#' 
#' IDdt <- data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                    Sort = c('IDQual', rep('IDDD', 4)),
#'                    Class = rep('character', 5),
#'                    Length = c('11', '20', '20', '20', '9'),
#'                    Qual1 = c('', rep('NumIdEst', 4)),
#'                    ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}'))
#' IDdt <- new(Class = 'DDdt', IDdt)
#' 
#' setID(DD) <- IDdt
#' DD
#' 
#' @rdname setID
#'
#' @import data.table
#'
#' @export
setGeneric("setID<-", function(object, value){standardGeneric("setID<-")})

#' @rdname setID
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setID",
    signature = c("DD", "DDdt"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))

        VNCaux <- getVNC(object)
        VNCaux[['ID']] <- new(Class = 'VNCdt')
        
        setVNC(object) <- DDdtToVNC(value, 'ID') + VNCaux
        object@ID <- value
        validObject(object)
        return(object)
    }
)
#' @rdname setID
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setID",
    signature = c("StQ", "DDdt"),
    function(object, value){
        
        setID(object@DD) <- value
        validObject(object)
        return(object)
    }
)
