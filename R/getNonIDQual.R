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
#' getNonIDQual(VNC)
#'
#' getNonIDQual(ExampleDD)
#' getNonIDQual(ExampleDD, 'MicroData')
#'
#' getNonIDQual(ExampleStQ)
#' getNonIDQual(ExampleStQ, 'Aggregates')
#'
#' @include VNC.R DD.R StQ.R getDD.R
#' 
#' @import data.table
#'
#' 
#' @export
setGeneric("getNonIDQual",
           function(object, CompNames = names(object)){standardGeneric("getNonIDQual")})

#' @rdname getNonIDQual
#'
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("VNC"),
    function(object, CompNames = names(object)){

        ValidComp <- names(object)
        NotValidComp <- CompNames[!CompNames %chin% ValidComp]
        if(!all(CompNames %in% ValidComp)) stop(paste0('[StQ::getNonIDQual] The following components are not present in the input object: ',
                                                       paste0(NotValidComp, collapse = ', '), '.\n'))

        NonIDQual.list <- lapply(object, function(DT){
            LocalOutput <- DT[['NonIDQual']]
            LocalOutput <- LocalOutput[LocalOutput != '']
            return(LocalOutput)
        })

        output <- unique(Reduce(c, NonIDQual.list, init = NonIDQual.list[[1]]))
        return(output)

    }
)

#' @rdname getNonIDQual
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("DD"),
    function(object, CompNames = setdiff(names(object), 'VNC')){

        CompNames <- setdiff(CompNames, 'VNC')
        output <- c()
        for (DDdt in CompNames) {

            NonIDQual <- object[[DDdt]][Sort == 'NonIDQual', Variable]
            NonIDQual <- NonIDQual[NonIDQual != '']
            output <- c(output, NonIDQual)
        }

        output <- unique(output)
        return(output)
    }
)

#' @rdname getNonIDQual
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("StQ"),
    function(object, CompNames = setdiff(names(getDD(object)), 'VNC')){

        output <- getNonIDQual(getDD(object), CompNames)
        return(output)
    }
)

#' @rdname getNonIDQual
#'
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("StQList"),
    function(object, CompNames){
        
        MissingComp <- missing(CompNames)
        output <- lapply(object$Data, function(StQ){
            
          if (MissingComp) CompNames <- setdiff(names(getDD(StQ)), 'VNC')
          out <- getNonIDQual(getDD(StQ), CompNames = CompNames)
          return(out)
        })
        return(output)
    }
)
