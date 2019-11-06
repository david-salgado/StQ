#' @title Return UnitNames from the input object
#'
#' @description \code{getUnitName} returns the UnitNames from input object.
#'
#' @param object Object whose UnitNames in the slot \link{DD} are required.
#'
#' @param CompNames \code{Character} vector with the names of components or slots of the object from
#'  which the UnitNames are requested.
#'
#' @return Returns a character vector with the UnitNames from the components specified in
#' CompNames. If no CompNames parameter is specified, it returns the UnitNames from all the
#' components of the input object.
#'
#' @examples
#' # An example:
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                 NonIDQual = c('','','','',''),
#'                                 IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                 NumIdEst = c('', rep('.', 4)),
#'                                 UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                                 InFiles = rep('FI', 5)),
#' MicroData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                        NonIDQual = c('', 'Market', ''),
#'                        IDDD = c(rep('', 2), 'NewOrders'),
#'                        NumIdEst = c(rep('', 2), '.'),
#'                        Market = c(rep('', 2), '1.'),
#'                        UnitName = c('numidest', '', 'cp09'),
#'                        InFiles = rep('FF', 3)),
#' ParaData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                       NonIDQual = c('', 'Action', ''),
#'                       IDDD = c(rep('', 2), 'Date'),
#'                       NumIdEst = c(rep('', 2), '.'),
#'                       Action = c(rep('', 2), 'Imputation'),
#'                       UnitName = c('numidest', '', 'FechaImput'),
#'                       InFiles = rep('FP', 3)),
#' AggWeights = data.table(IDQual = c('CCAA', 'NACE09', ''),
#'                         NonIDQual = rep('', 3),
#'                         IDDD = c('', '', 'Ponderacion'),
#'                         CCAA = c('', '', '.'),
#'                         NACE09 = c('', '', '.'),
#'                         UnitName = c('Provincia', '', ''),
#'                         InFiles = rep('FF', 3)))
#' 
#' VNC <- BuildVNC(VarList)
#' 
#' getUnitName(VNC)
#'
#'
#' @export
setGeneric("getUnitName", function(object, CompNames = names(object)){standardGeneric("getUnitName")})

#' @rdname getUnitName
#'
#' @include VNC.R DD.R StQ.R getVNC.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getUnitName",
    signature = c("VNC"),
    function(object, CompNames = names(object)){
        
        ValidComp <- names(object)
        NotValidComp <- CompNames[!CompNames %in% ValidComp]
        if(!all(CompNames %in% ValidComp)) stop(paste0('[StQ::getUnitName] The following components are not present in the input object: ',
                                                       paste0(NotValidComp, collapse = ', '), '.\n'))
        
        IDDD.list <- lapply(object, function(DT){
            LocalOutput <- DT[['UnitName']]
            LocalOutput <- LocalOutput[LocalOutput != '']
            return(LocalOutput)
        })
        
        output <- unique(Reduce(c, IDDD.list, init = IDDD.list[[1]]))
        return(output)
        
    }
)

#' @rdname getUnitName
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getUnitName",
    signature = c("DD"),
    function(object, CompNames){
        
        VNC <- getVNC(object)
        mc <- match.call()
        mc[['object']] <- VNC
        output <- eval(mc)
        return(output)
    }
)

#' @rdname getUnitName
#'
#' @export
setMethod(
    f = "getUnitName",
    signature = c("StQ"),
    function(object, CompNames){
        
        VNC <- getVNC(object)
        mc <- match.call()
        mc[['object']] <- VNC
        output <- eval(mc)
        return(output)
    }
)
