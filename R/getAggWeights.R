#' @title Return slot \code{AggWeights} from a \link{DD} object
#'
#' @description \code{getAggWeights} returns slot \code{AggWeights} of the input \link{DD} 
#' object.
#' 
#' @param object A DD Object whose slot \code{AggWeights} is queried.
#'
#' @return \linkS4class{data.table} with data from slot \code{AggWeights} of the input 
#' \link{DD} object.
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
#' getAggWeights(VNC)
#' 
#' @include VNC.R DD.R
#' 
#' @export
setGeneric("getAggWeights", function(object){standardGeneric("getAggWeights")})

#' @rdname getAggWeights
#' 
#' @export
setMethod(
    f = "getAggWeights",
    signature = c("VNC"),
    function(object){object[['AggWeights']]}
)

#' @rdname getAggWeights
#' 
#' @export
setMethod(
    f = "getAggWeights",
    signature = c("DD"),
    function(object){object[['AggWeights']]}
)
