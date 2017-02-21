#' @title Return IDDD identifiers (statistical variable names) from the slot DD of an object
#'
#' @description \code{getIDDD} returns the IDDD identifiers from the slot DD of the input object.
#' Notice that not all returned IDDD identifiers may be present in the input object.
#'
#' @param object Object whose IDDD identifiers in the slot \linkS4class{DD} are required.
#'
#' @param CompNames \code{Character} vector with the names of components or slots of the object from
#'  which the IDDD identifiers are requested.
#'
#' @return Returns a character vector with the IDDD identifiers from the components specified in
#' CompNames. If no CompNames parameter is specified, it returns the IDDD identifiers from all the
#' components of the input object.
#'
#' @examples
#' library(data.table)
#' VarList <- list(ID = new(Class = 'VNCdt',
#'                 data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                            NonIDQual = c('','','','',''),
#'                            IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                            NumIdEst = c('', rep('.', 4)),
#'                            UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                            InFiles = rep('FI', 5))),
#' MicroData =new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                            NonIDQual = c('', 'Market', ''),
#'                                            IDDD = c(rep('', 2), 'NewOrders'),
#'                                            NumIdEst = c(rep('', 2), '.'),
#'                                            Market = c(rep('', 2), '1.'),
#'                                            UnitName = c('numidest', '', 'cp09'),
#'                                            InFiles = rep('FF, FD, FG', 3))),
#' ParaData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                            NonIDQual = c('', 'Action', ''),
#'                                            IDDD = c(rep('', 2), 'Date'),
#'                                            NumIdEst = c(rep('', 2), '.'),
#'                                            Action = c(rep('', 2), 'Imputation'),
#'                                            UnitName = c('numidest', '', 'FechaImput'),
#'                                            InFiles = rep('FP', 3))),
#' AggWeights = new(Class = 'VNCdt', data.table(IDQual = c('CCAA', 'NACE09', ''),
#'                                            NonIDQual = rep('', 3),
#'                                            IDDD = c('', '', 'Ponderacion'),
#'                                            CCAA = c('', '', '.'),
#'                                            NACE09 = c('', '', '.'),
#'                                            UnitName = c('Provincia', '', ''),
#'                                            InFiles = rep('FA', 3))))
#'
#' VNC <- new(Class = 'VarNameCorresp', .Data = VarList)
#' getIDDD(VNC)
#'
#'
#' @export
setGeneric("getIDDD", function(object, CompNames = names(object)){standardGeneric("getIDDD")})

#' @rdname getIDDD
#'
#' @include VNCdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDDD",
    signature = c("VNCdt"),
    function(object, CompNames){

        output <- unique(object[['IDDD']])
        output <- output[output != '']
        return(output)

    }
)
#' @rdname getIDDD
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDDD",
    signature = c("VarNameCorresp"),
    function(object, CompNames){

        ValidComp <- names(object)
        NotValidComp <- CompNames[!CompNames %in% ValidComp]
        if(!all(CompNames %in% ValidComp)) stop(paste0('[StQ::getIDDD] The following components are not present in the input object: ',
                                                       paste0(NotValidComp, collapse = ', '), '.\n'))

        aux <- object[CompNames]
        IDDD.list <- lapply(aux, function(x){getIDDD(x, CompNames)})

        output <- unique(Reduce(c, IDDD.list, init = IDDD.list[[1]]))
        return(output)

    }
)

#' @rdname getIDDD
#'
#' @include DDdt-class.R DatadtToDT.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDDD",
    signature = c("DDdt"),
    function(object, CompNames){

        output <- unique(DatadtToDT(object)[Sort == 'IDDD', Variable])
        output <- output[output != '']

        return(output)
    }
)

#' @rdname getIDDD
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDDD",
    signature = c("DD"),
    function(object, CompNames = setdiff(slotNames(object), 'VarNameCorresp')){

        output <- c()
        for (slotDD in CompNames) {

            IDDD <- getIDDD(slot(object, slotDD), slotDD)
            output <- c(output, IDDD)
        }

        output <- unique(output)
        return(output)
    }
)

#' @rdname getIDDD
#'
#' @include StQ-class.R getDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDDD",
    signature = c("StQ"),
    function(object, CompNames = setdiff(slotNames(getDD(object)), 'VarNameCorresp')){

        output <- getIDDD(getDD(object), CompNames)
        return(output)
    }
)

#' @rdname getIDDD
#'
#' @include rawStQ-class.R getDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDDD",
    signature = c("rawStQ"),
    function(object, CompNames = setdiff(slotNames(getDD(object)), 'VarNameCorresp')){

        output <- getIDDD(getDD(object), CompNames)
        return(output)
    }
)
