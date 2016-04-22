#' @title Set value of slot \code{VarNameCorresp}.
#'
#' @description \code{setVNC} assigns a list of \linkS4class{VNCdt}s to the
#'  slot \code{VarNameCorresp} of the input object.
#'
#' @param object Object containing slot \code{VarNameCorresp} to be assigned.
#'
#' @param value List of \linkS4class{VNCdt}s to be assigned to the slot 
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
#' setVNC(DD) <- VarList
#' DD
#' str(DD)
#'
#' # On an object of class StQ:
#' library(data.table)
#' data(ExampleQ)
#' VarList <- list(Aggregates = new(Class = 'VNCdt',
#'                                 data.table(IDQual = c('Province','NACE09',
#'                                                       'IsNatMarket',''),
#'                                            NonIDQual = rep('', 4),
#'                                            IDDD = c('', '', '' ,'Turnover'),
#'                                            Province = c('', '', '', '.'),
#'                                            NACE09 = c('', '', '', '.'),
#'                                            IsNatMarket = c('', '', '', '1'),
#'                                            Unit1 = c('provincia', 'actividad',
#'                                                      '', 'cn01'))))
#' setVNC(ExampleQ) <- VarList
#' ExampleQ
#' str(ExampleQ)
#' 
#' @rdname setVNC
#'
#' @import data.table
#'
#' @export
setGeneric("setVNC<-", function(object, value){standardGeneric("setVNC<-")})

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
        
        classValue <- unique(unlist(lapply(value, class)))
        if(length(classValue) > 1 | classValue != 'VNCdt'){
            stop('[DD::setVNC] Value assigned to the slot VarNamecorresp of a DD object must be a list of VNCdt objects.')
        }
        object@VarNameCorresp@.Data <- value
        setNames(object@VarNameCorresp@.Data, names(value))
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
