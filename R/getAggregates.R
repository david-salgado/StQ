#' @title Return slot Aggregates from the input object
#'
#' @description \code{getAggregates} returns slot \code{Aggregates} of the input \link{DD} object.
#' 
#' @param object A DD Object whose slot \code{Aggregates} is queried.
#'
#' @return \linkS4class{data.table} with data from slot \code{Aggregates} of the
#' input \link{DD} object.
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
#' getAggregates(VNC)
#' 
#' @include VNC.R DD.R
#' 
#' @export
setGeneric("getAggregates", function(object){standardGeneric("getAggregates")})

#' @rdname getAggregates
#' 
#' 
#' @export
setMethod(
    f = "getAggregates",
    signature = c("VNC"),
    function(object){object[['Aggregates']]}
)

#' @rdname getAggregates
#' 
#' 
#' @export
setMethod(
    f = "getAggregates",
    signature = c("DD"),
    function(object){object[['Aggregates']]}
)
