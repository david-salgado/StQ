#' @title Return unit qualifiers from an object
#'
#' @description \code{getIDQual} returns a character vector with all unit qualifier names (IDQual)
#' contained in the input object.
#'
#' @param object Object with the unit qualifiers (IDQual) to be queried.
#'
#' @param CompNames Character vector with the components or slots from which IDQuals are queried.
#'
#' @return Character vector with all the unit qualifier names.
#'
#' @details Unit qualifiers are those qualifiers identifying statistical units.
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
#' getIDQual(VNC)
#' getIDQual(VNC, 'MicroData')
#' getIDQual(VNC, 'ParaData')
#'
#' @export
setGeneric("getIDQual", function(object, CompNames = names(object)){standardGeneric("getIDQual")})

#' @rdname getIDQual
#'
#' @include VNC.R DD.R StQ.R getDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("VNC"),
    function(object, CompNames = names(object)){

        ValidComp <- names(object)
        NotValidComp <- CompNames[!CompNames %chin% ValidComp]
        if(!all(CompNames %in% ValidComp)) stop(paste0('[StQ::getIDQual] The following components are not present in the input object: ',
                                                       paste0(NotValidComp, collapse = ', '), '.\n'))
        
        
        IDQual.list <- lapply(CompNames, function(CompName) {
            DT <- object[[CompName]]
            LocalOutput <- DT[['IDQual']]
            LocalOutput <- LocalOutput[LocalOutput != '']
            return(LocalOutput)
        })
        output <- unique(Reduce(c, IDQual.list, init = IDQual.list[[1]]))
        return(output)

    }
)

#' @rdname getIDQual
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("DD"),
    function(object, CompNames = setdiff(names(object), 'VNC')){

        output <- c()
        for (DDdt in setdiff(CompNames, 'VNC')) {

            IDQual <- object[[DDdt]][Sort == 'IDQual', Variable]
            IDQual <- IDQual[IDQual != '']
            output <- c(output, IDQual)
        }

        output <- unique(output)
        return(output)
    }
)

#' @rdname getIDQual
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("StQ"),
    function(object, CompNames = setdiff(names(getDD(object)), 'VNC')){

        output <- getIDQual(getDD(object), CompNames)
        return(output)
    }
)
