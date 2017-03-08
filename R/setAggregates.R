#' @title Set value of slot \code{Aggregates}
#'
<<<<<<< HEAD
#' @description \code{setAggregates} assigns a \linkS4class{data.table} to the slot \code{Aggregates} of 
=======
#' @description \code{setAggregates} assigns a \linkS4class{DDdt} to the slot \code{Aggregates} of 
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' the input object.
#'
#' @param object Object whose slot \code{Aggregates} is to be assigned.
#'
<<<<<<< HEAD
#' @param value \linkS4class{data.table} to be assigned to the slot \code{Aggregates}.
=======
#' @param value \linkS4class{DDdt} to be assigned to the slot \code{Aggregates}.
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @return Object with slot Aggregates updated.
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
#' DD <- BuildDD(list(VNC = VNC, MicroData = MicroDataDD))
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
#' Aggdt <- data.table(Variable = c('NACE09', 'Turnover'),
#'                     Sort = c('IDQual', 'IDDD'),
#'                     Class = c('character', 'character'),
#'                     Length = c('4', '10'),
#'                     Qual1 = c('', 'NACE09'),
#'                     ValueRegExp = c('([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )'))  
<<<<<<< HEAD
=======
#' Aggdt <- new(Class = 'DDdt', Aggdt)          
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' setAggregates(DD) <- Aggdt
#' DD
#'
#' @rdname setAggregates
#'
#' @import data.table
<<<<<<< HEAD
#' 
#' @include VNC.R DD.R getVNC.R setVNC.R StQ.R getAggregates.R DDdtToVNC.R plus.VNC.R
#'
#' @export
setGeneric("setAggregates<-", function(object, value){standardGeneric("setAggregates<-")})

#' @rdname setAggregates
#'
=======
#'
#' @export
setGeneric("setAggregates<-", function(object, value){standardGeneric("setAggregates<-")})
#' @rdname setAggregates
#'
#' @include DD-class.R getVNC.R DDdtToVNC.R setVNC.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setAggregates",
<<<<<<< HEAD
    signature = c("VNC", "data.table"),
    function(object, value){
        
        VNCValue <- try(BuildVNC(value))
        if (inherits(VNCValue, "try-error")) stop('[StQ::setAggregates] Value does not have the correct format.\n')
        object[['Aggregates']] <- value
=======
    signature = c("DD", "DDdt"),
    function(object, value){
        
        setkeyv(value, setdiff(names(value), 'Value'))

        VNCaux <- getVNC(object)
        VNCaux[['Aggregates']] <- new(Class = 'VNCdt')
        
        setVNC(object) <- DDdtToVNC(value, 'Aggregates') + VNCaux
        object@Aggregates <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)
#' @rdname setAggregates
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
    f = "setAggregates",
<<<<<<< HEAD
    signature = c("DD", "data.table"),
    function(object, value){
        
        newVNC <- DDdtToVNC(value, NameVNC = 'Aggregates', InFiles = 'FA')
        DDValue <- try(BuildDD(list(VNC = newVNC, Aggregates = value)))
        if (inherits(DDValue, "try-error")) stop('[StQ::setAggregates] Value does not have the correct format.\n')
        object[['VNC']] <- object[['VNC']] + newVNC
        object[['Aggregates']] <- value
=======
    signature = c("StQ", "DDdt"),
    function(object, value){
        
        setAggregates(object@DD) <- value
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)
