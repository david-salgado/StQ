#' @title Return IDDD identifiers (statistical variable names) from the input object
#'
#' @description \code{getIDDD} returns the IDDD identifiers from input object.
#'
#' @param object Object whose IDDD identifiers in the slot \link{DD} are required.
#'
#' @param CompNames \code{Character} vector with the names of components or slots of the object from
#'  which the IDDD identifiers are requested.
#'
#' @return Returns a character vector with the IDDD identifiers from the components specified in
#' CompNames. If no CompNames parameter is specified, it returns the IDDD identifiers from all the
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
#' getIDDD(VNC)
#'
#'
#' @export
setGeneric("getIDDD", function(object, CompNames = names(object)){standardGeneric("getIDDD")})

#' @rdname getIDDD
#'
#' @include VNC.R DD.R StQ.R getData.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDDD",
    signature = c("VNC"),
    function(object, CompNames = names(object)){

        ValidComp <- names(object)
        NotValidComp <- CompNames[!CompNames %in% ValidComp]
        if(!all(CompNames %in% ValidComp)) stop(paste0('[StQ::getIDDD] The following components are not present in the input object: ',
                                                       paste0(NotValidComp, collapse = ', '), '.\n'))

        IDDD.list <- lapply(object, function(DT){
            LocalOutput <- DT[['IDDD']]
            LocalOutput <- LocalOutput[LocalOutput != '']
            return(LocalOutput)
        })

        output <- unique(Reduce(c, IDDD.list, init = IDDD.list[[1]]))
        return(output)

    }
)

#' @rdname getIDDD
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDDD",
    signature = c("DD"),
    function(object, CompNames = setdiff(names(object), 'VNC')){

        output <- c()
        for (slotDD in setdiff(CompNames, 'VNC')) {

            IDDD <- object[[slotDD]][Sort == 'IDDD', Variable]
            IDDD <- IDDD[IDDD != '']
            output <- c(output, IDDD)
        }
        output <- unique(output)
        return(output)
    }
)

#' @rdname getIDDD
#'
#' @export
setMethod(
    f = "getIDDD",
    signature = c("StQ"),
    function(object, CompNames = setdiff(names(getDD(object)), 'VNC')){

        output <- getIDDD(getDD(object), CompNames)
        return(output)
    }
)
