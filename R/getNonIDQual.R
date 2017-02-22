#' @title Return non-unit qualifiers from an object
#'
#' @description \code{getNonIDQual} returns a character vector with all non-unit qualifier names
#' (NonIDQual) contained in the input object.
#'
#' @param object Object with the non-unit variable qualifiers (NonIDQual) to be queried.
#'
#' @param CompNames Character vector with the components or slots from which NonIDQuals are queried.
#'
#' @return Character vector with all the non-unit qualifier names.
#'
#' @details Non-unit qualifiers are those qualifiers used to compose statistical variable names.
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
#' VNC <- new(Class = 'VarNameCorresp', .Data = VarList)
#' getNonIDQual(VNC)
#'
#' getNonIDQual(ExampleDD)
#' getNonIDQual(ExampleDD, 'MicroData')
#'
#' getNonIDQual(ExampleStQ)
#' getNonIDQual(ExampleStQ, 'Aggregates')
#'
#' @export
setGeneric("getNonIDQual",
           function(object, CompNames = names(object)){standardGeneric("getNonIDQual")})

#' @rdname getNonIDQual
#'
#' @include VNCdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("VNCdt"),
    function(object){

        output <- unique(object[['NonIDQual']])
        output <- output[output != '']
        return(output)

    }
)
#' @rdname getNonIDQual
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("VarNameCorresp"),
    function(object, CompNames = names(object)){

        ValidComp <- names(object)
        NotValidComp <- CompNames[!CompNames %in% ValidComp]
        if(!all(CompNames %in% ValidComp)) stop(paste0('[StQ::getNonIDQual] The following components are not present in the input object: ',
                                                       paste0(NotValidComp, collapse = ', '), '.\n'))

        aux <- object[CompNames]

        NonIDQual.list <- lapply(aux, function(x){getNonIDQual(x)})

        output <- unique(Reduce(c, NonIDQual.list, init = NonIDQual.list[[1]]))
        return(output)

    }
)

#' @rdname getNonIDQual
#'
#' @include DDdt-class.R DatadtToDT.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("DDdt"),
    function(object){

        output <- unique(DatadtToDT(object)[Sort == 'NonIDQual', Variable])
        output <- output[output != '']

        return(output)
    }
)

#' @rdname getNonIDQual
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("DD"),
    function(object, CompNames = setdiff(slotNames(object), 'VarNameCorresp')){

        if (missing(CompNames)) CompNames <- slotNames(object)

        CompNames <- setdiff(CompNames, 'VarNameCorresp')
        output <- c()
        for (DDdt in CompNames) {

            NonIDQual <- getNonIDQual(slot(object,DDdt))
            output <- c(output, NonIDQual)
        }

        output <- unique(output)
        return(output)
    }
)

#' @rdname getNonIDQual
#'
#' @include StQ-class.R getDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("StQ"),
    function(object, CompNames = setdiff(slotNames(getDD(object)), 'VarNameCorresp')){

        output <- getNonIDQual(getDD(object), CompNames)
        return(output)
    }
)

#' @rdname getNonIDQual
#'
#' @include rawStQ-class.R getDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("rawStQ"),
    function(object, CompNames = setdiff(slotNames(getDD(object)), 'VarNameCorresp')){

        output <- getNonIDQual(getDD(object), CompNames)
        return(output)
    }
)
