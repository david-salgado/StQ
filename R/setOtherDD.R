#' @title Set value of slot \code{Other}
#'
#' @description \code{setOtherDD} assigns a \linkS4class{DDdt} to the slot
#' \code{Other} of the input object.
#'
#' @param object Object whose slot \code{Other} is to be assigned.
#'
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{Other}.
#'
#' @return Object with slot Other updated.
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
#' Otherdt <- data.table(Variable = c('NACE08'), Sort = c('NonIDQual'), Class = c('character'),
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
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setOtherDD",
    signature = c("DD", "DDdt"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))
        if (dim(object@Other)[1] > 0){
            
            object@Other <- new(Class = 'DDdt')
            setVNC(object) <- DDdtToVNC(value, 'Other')
            
        }else{
            
            setVNC(object) <- DDdtToVNC(value, 'Other') + getVNC(object)
        }
        
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
