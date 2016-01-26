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
#' # On an empty object of class VarNameCorresp:
#' VNC <- new(Class = 'VarNameCorresp')
#' VarList <- list(MicroData = data.table(IDQual = c('NumIdEst','','','',''),
#'                                        NonIDQual = c('EsMercNac', 
#'                                                      'EsMercEuro', 
#'                                                      'EsMercRM',
#'                                                      'Cod',
#'                                                      ''),
#'                                        IDDD = c('', '', '' ,'' ,
#'                                                 'IEPEntradaPed'),
#'                                        Unit1 = c('', '', '', '', 'cp02')))
#' setVNC(VNC) <- VarList
#' VNC
#' str(VNC)
#' 
#' # On an object of class DD:
#' MicroDataDD <- data.table(Variable = 'IEPEntradaPed', 
#'                           Sort = 'IDDD', 
#'                           Class = 'numeric',
#'                           Qual1 = 'NOrden')
#' DD <- new(Class = 'DD', MicroData = MicroDataDD)
#' VarList <- list(MicroData = data.table(IDQual = c('NumIdEst','','','',''),
#'                                        NonIDQual = c('EsMercNac', 
#'                                                      'EsMercEuro', 
#'                                                      'EsMercRM',
#'                                                      'Cod',
#'                                                      ''),
#'                                        IDDD = c('', '', '' ,'' ,
#'                                                 'IEPEntradaPed'),
#'                                        Unit1 = c('', '', '', '', 'cp02')))
#' setVNC(DD) <- VarList
#' DD
#' str(DD)
#'
#' # On an object of class StQ:
#' MicroDataDD <- data.table(Variable = 'IEPEntradaPed', 
#'                           Sort = 'IDDD', 
#'                           Class = 'numeric',
#'                           Qual1 = 'NOrden')
#' DD <- new(Class = 'DD', MicroData = MicroDataDD)
#' StQ <- new(Class = 'StQ', 
#'            Data = data.table(IDDD = character(0), Value = numeric(0)), 
#'            DD = DD)
#' VarList <- list(MicroData = data.table(IDQual = c('NumIdEst','','','',''),
#'                                        NonIDQual = c('EsMercNac', 
#'                                                      'EsMercEuro', 
#'                                                      'EsMercRM',
#'                                                      'Cod',
#'                                                      ''),
#'                                        IDDD = c('', '', '' ,'' ,
#'                                                 'IEPEntradaPed'),
#'                                        Unit1 = c('', '', '', '', 'cp02')))
#' setVNC(StQ) <- VarList
#' StQ
#' str(StQ)
#' 
#' @rdname setVNC
#'
#' @import data.table
#'
#' @export
setGeneric("setVNC<-", function(object, value){standardGeneric("setVNC<-")})

#' @rdname setVNC
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setVNC",
    signature = c("VarNameCorresp", "list"),
    function(object, value){
        
        object@VarNameCorresp <- value
        validObject(object)
        return(object)
    }
)
#' @rdname setVNC
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setVNC",
    signature = c("DD", "list"),
    function(object, value){
        
        setVNC(object@VarNameCorresp) <- value
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
    signature = c("StQ", "list"),
    function(object, value){
        
        setVNC(object@DD) <- value
        validObject(object)
        return(object)
    }
)
