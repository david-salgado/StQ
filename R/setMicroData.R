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
#'                           Qual1 = 'NumIdEst', Qual2 = 'EsMercNac', Qual3 = 'EsMercEuro',
#'                           Qual4 = 'EsMercRM', ValueRegExp = '')
#' MicroDataDD <- new(Class = 'DDdt', MicroDataDD)
#' VarList <- list(MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst','','','',''),
#'                                                             NonIDQual = c('EsMercNac', 
#'                                                                           'EsMercEuro', 
#'                                                                           'EsMercRM', 'Cod', ''),
#'                                                             IDDD = c('', '', '' ,'' ,
#'                                                                      'IEPEntradaPed'),
#'                                                             NumIdEst = c('', '', '', '', '.'),
#'                                                             EsMercNac = c('', '', '', '', '0'),
#'                                                             EsMercEuro = c('', '', '', '', '0'),
#'                                                             EsMercRM = c('', '', '', '', '1'),
#'                                                             Cod = rep('', 5),
#'                                                             Unit1 = c('', '', '', '', 'cp02'))))
#' VNC <- BuildVNC(VarList)
#' DD <- new(Class = 'DD', VarNameCorresp = VNC, MicroData = MicroDataDD)
#' 
#' Microdt <- data.table(Variable = c('NumIdEst', 'NewOrders'),
#'                       Sort = c('IDQual', 'IDDD'),
#'                       Class = c('character', 'character'),
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
        if (dim(object@MicroData)[1] > 0){
            
            object@MicroData <- new(Class = 'DDdt')
            setVNC(object) <- DDdtToVNC(value, 'MicroData')
            
        }else{
            
            setVNC(object) <- DDdtToVNC(value, 'MicroData') + getVNC(object)
        }
        
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
