#' @title Set value of slot \code{ID} of an object \linkS4class{DD}
#'
#' @description \code{setID} assigns a \linkS4class{data.table} to the
#'  slot \code{ID} of the input object.
#'
#' @param object Object containing slot \code{ID} to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{ID}.
#'
#' @return Object \linkS4class{DD} with slot \code{ID} updated.
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
#' VNC[['MicroData']] <- VarList
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
#' setID(DD) <- VarList
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
#'            Data = data.table(IDDD = character(0), Value = character(0)), 
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
#' setID(StQ) <- VarList
#' StQ
#' str(StQ)
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
    signature = c("DD", "data.table"),
    function(object, value){
        
        object@ID <- value
        validObject(object)
        return(object)
    }
)
