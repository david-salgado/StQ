#' @title Set value of slot \code{AggWeights}
#'
#' @description \code{setAggrWeights} assigns a \linkS4class{DDdt} to the slot \code{AggrWeights} of the input object.
#'
#' @param object Object whose slot \code{AggWeights} is to be assigned.
#'
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{AggWeights}.
#'
#' @return Object with slot AggWeights updated.
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
#' AggWeights <- data.table(Variable = c('ID', 'Pond1'), Sort = c('IDQual', 'IDDD'),
#'                          Class = c('character', 'character'),
#'                          Length = c('11', '8'),
#'                          Qual1 = c('', 'ID'), ValueRegExp = c('', ''))
<<<<<<< HEAD
=======
#' AggWeights <- new(Class = 'DDdt', AggWeights)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' 
#' setAggWeights(DD) <- AggWeights
#' DD
#'
#' @rdname setAggWeights
#'
#' @import data.table
<<<<<<< HEAD
#' 
#' @include VNC.R DD.R getVNC.R DDdtToVNC.R
#'
#' @export
setGeneric("setAggWeights<-", function(object, value){standardGeneric("setAggWeights<-")})

#' @rdname setAggWeights
#'
=======
#'
#' @export
setGeneric("setAggWeights<-", function(object, value){standardGeneric("setAggWeights<-")})
#' @rdname setAggWeights
#'
#' @include DD-class.R getVNC.R DDdtToVNC.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setAggWeights",
<<<<<<< HEAD
    signature = c("VNC", "data.table"),
    function(object, value){
        
        VNCValue <- try(BuildVNC(value))
        if (inherits(VNCValue, "try-error")) stop('[StQ::setAggWeights] Value does not have the correct format.\n')
        object[['AggWeights']] <- value
=======
    signature = c("DD", "DDdt"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))

        VNCaux <- getVNC(object)
        VNCaux[['AggWeights']] <- new(Class = 'VNCdt')
        
        setVNC(object) <- DDdtToVNC(value, 'AggWeights') + VNCaux
        object@AggWeights <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)
#' @rdname setAggWeights
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
    f = "setAggWeights",
<<<<<<< HEAD
    signature = c("DD", "data.table"),
    function(object, value){
        
        newVNC <- DDdtToVNC(value, NameVNC = 'AggWeights', InFiles = 'FF')
        DDValue <- try(BuildDD(list(VNC = newVNC, AggWeights = value)))
        if (inherits(DDValue, "try-error")) stop('[StQ::Aggregates] Value does not have the correct format.\n')
        object[['VNC']] <- object[['VNC']] + newVNC
        object[['AggWeights']] <- value
=======
    signature = c("StQ", "DDdt"),
    function(object, value){
        
        setAggWeights(object@DD) <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)
