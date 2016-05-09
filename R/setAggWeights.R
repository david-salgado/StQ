#' @title Set value of slot \code{AggWeights}.
#'
#' @description \code{setAggrWeights} assigns a \linkS4class{DDdt} to the slot
#' \code{AggrWeights} of the input object.
#'
#' @param object Object whose slot \code{AggWeights} is to be assigned.
#'
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{AggWeights}.
#'
#' @return Object with slot AggWeights updated.
#'
#' @examples
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
#' AggWeights <- data.table(Variable = c('ID', 'Pond1'), Sort = c('IDQual', 'IDDD'),
#'                          Class = c('character', 'character'),
#'                          Qual1 = c('', 'ID'), ValueRegExp = c('', ''))
#' AggWeights <- new(Class = 'DDdt', AggWeights)
#' 
#' setAggWeights(DD) <- AggWeights
#' DD
#'
#' @rdname setAggWeights
#'
#' @import data.table
#'
#' @export
setGeneric("setAggWeights<-", function(object, value){standardGeneric("setAggWeights<-")})
#' @rdname setAggWeights
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setAggWeights",
    signature = c("DD", "DDdt"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))
        if (dim(object@AggWeights)[1] > 0){
            
            object@AggWeights <- new(Class = 'DDdt')
            setVNC(object) <- DDdtToVNC(value, 'AggWeights')
            
        }else{
            
            setVNC(object) <- DDdtToVNC(value, 'AggWeights') + getVNC(object)
        }
        
        object@AggWeights <- value
        validObject(object)
        return(object)
    }
)
#' @rdname setAggWeights
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setAggWeights",
    signature = c("StQ", "DDdt"),
    function(object, value){
        
        setAggWeights(object@DD) <- value
        validObject(object)
        return(object)
    }
)
