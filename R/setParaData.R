#' @title Set value of slot \code{ParaData}.
#'
#' @description \code{setParaData} assigns a \linkS4class{DDdt} to the slot
#' \code{ParaData} of the input object.
#'
#' @param object Object whose slot \code{ParaData} is to be assigned.
#'
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{ParaData}.
#'
#' @return Object with slot ParaData updated.
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
#'                                                           Unit1 = c('', '', '', 'cp02'))))
#' VNC <- BuildVNC(VarList)
#' DD <- new(Class = 'DD', VarNameCorresp = VNC, MicroData = MicroDataDD)
#'           
#' Paradt <- data.table(
#'              Variable = c('NumIdEst', 'Date'),
#'              Sort = c('IDQual', 'IDDD'),
#'              Class = rep('character', 2),
#'              Length = c('11', '8'),
#'              Qual1 = c('', 'NumIdEst'),
#'              ValueRegExp = c('[0-9]{9}PP', 
#'                              '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )'))       
#' Paradt <- new(Class = 'DDdt', Paradt)          
#' setParaData(DD) <- Paradt
#' DD
#' 
#' @rdname setParaData
#'
#' @import data.table
#'
#' @export
setGeneric("setParaData<-", function(object, value){standardGeneric("setParaData<-")})
#' @rdname setParaData
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setParaData",
    signature = c("DD", "DDdt"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))
        if (dim(object@ParaData)[1] > 0){
            
            object@ParaData <- new(Class = 'DDdt')
            setVNC(object) <- DDdtToVNC(value, 'ParaData')
            
        }else{
            
            setVNC(object) <- DDdtToVNC(value, 'ParaData') + getVNC(object)
        }
        
        object@ParaData <- value
        validObject(object)
        return(object)
    }
)
#' @rdname setParaData
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setParaData",
    signature = c("StQ", "DDdt"),
    function(object, value){
        
        setParaData(object@DD) <- value
        validObject(object)
        return(object)
    }
)
