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
<<<<<<< HEAD
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
#' getIDQual(VNC, 'ParaData')
=======
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
#' getIDQual(VNC)
#'
#' getIDQual(ExampleDD)
#' getIDQual(ExampleDD, 'MicroData')
#'
#' getIDQual(ExampleStQ)
#' getIDQual(ExampleStQ, 'Aggregates')
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @export
setGeneric("getIDQual", function(object, CompNames = names(object)){standardGeneric("getIDQual")})

#' @rdname getIDQual
#'
<<<<<<< HEAD
#' @include VNC.R DD.R StQ.R
=======
#' @include VNCdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("VNCdt"),
    function(object, CompNames){

        output <- unique(object[['IDQual']])
        output <- output[output != '']
        return(output)

    }
)
#' @rdname getIDQual
#'
#' @include VarNameCorresp-class.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
<<<<<<< HEAD
    signature = c("VNC"),
=======
    signature = c("VarNameCorresp"),
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
    function(object, CompNames = names(object)){

        ValidComp <- names(object)
        NotValidComp <- CompNames[!CompNames %in% ValidComp]
        if(!all(CompNames %in% ValidComp)) stop(paste0('[StQ::getIDQual] The following components are not present in the input object: ',
                                                       paste0(NotValidComp, collapse = ', '), '.\n'))

<<<<<<< HEAD
        IDQual.list <- lapply(object, function(DT) {
            LocalOutput <- DT[['IDQual']]
            LocalOutput <- LocalOutput[LocalOutput != '']
            return(LocalOutput)
        })
=======
        aux <- object[CompNames]

        IDQual.list <- lapply(aux, function(x) {getIDQual(x)})

>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        output <- unique(Reduce(c, IDQual.list, init = IDQual.list[[1]]))
        return(output)

    }
)

#' @rdname getIDQual
#'
<<<<<<< HEAD
=======
#' @include DDdt-class.R DatadtToDT.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("DDdt"),
    function(object, CompNames){

        output <- unique(DatadtToDT(object)[Sort == 'IDQual', Variable])
        output <- output[output != '']

        return(output)
    }
)

#' @rdname getIDQual
#'
#' @include DD-class.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("DD"),
<<<<<<< HEAD
    function(object, CompNames = setdiff(names(object), 'VNC')){

        output <- c()
        for (DDdt in setdiff(CompNames, 'VNC')) {

            IDQual <- object[[DDdt]][Sort == 'IDQual', Variable]
            IDQual <- IDQual[IDQual != '']
=======
    function(object, CompNames = setdiff(slotNames(object), 'VarNameCorresp')){

        output <- c()
        for (DDdt in CompNames) {

            IDQual <- getIDQual(slot(object, DDdt))
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            output <- c(output, IDQual)
        }

        output <- unique(output)
        return(output)
    }
)

#' @rdname getIDQual
#'
<<<<<<< HEAD
=======
#' @include StQ-class.R getDD.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("StQ"),
<<<<<<< HEAD
    function(object, CompNames = setdiff(slotNames(getDD(object)), 'VNC')){
=======
    function(object, CompNames = setdiff(slotNames(getDD(object)), 'VarNameCorresp')){

        output <- getIDQual(getDD(object), CompNames)
        return(output)
    }
)

#' @rdname getIDQual
#'
#' @include rawStQ-class.R getDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("rawStQ"),
    function(object, CompNames = setdiff(slotNames(getDD(object)), 'VarNameCorresp')){
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8

        output <- getIDQual(getDD(object), CompNames)
        return(output)
    }
)