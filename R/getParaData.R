#' @title Return slot ParaData from an object
#'
#' @description \code{getParaData} returns slot \code{ParaData} of the input object.
#' 
#' @param object a DD Object whose slot \code{ParaData} is queried.
#'
#' @return Returns a \linkS4class{DDdt} object containing the slot \code{ParaData} of the input object.
#'
#' @examples
#' # An example:
#' library(data.table)
#' ### We build the VNC object
<<<<<<< HEAD
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
#' getParaData(VNC)
#' 
#' @include VNC.R DD.R
=======
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
#' VNC <- new(Class = 'VarNameCorresp', VarList)
#' 
#' ### We build the specification data.tables
#' IDdt <- new(Class='DDdt',
#'   data.table(
#'     Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'     Sort = c('IDQual', rep('IDDD', 4)),
#'     Class = rep('character', 5),
#'     Length = c('11', '15', '15', '20','9'),
#'     Qual1 = c('', rep('NumIdEst', 4)),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')))
#'     
#' Microdt <- new(Class='DDdt',
#'   data.table(
#'     Variable = c('NumIdEst', 'Market', 'NewOrders'),
#'     Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'     Class = c(rep('character', 2), 'numeric'),
#'     Length = c('11', '2', '7'),
#'     Qual1 = c(rep('', 2), 'NumIdEst'),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '([0-9]{1, 10}| )')))
#'     
#' Paradt <-new(Class='DDdt', 
#'   data.table(
#'     Variable = c('NumIdEst', 'Action', 'Date'),
#'     Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'     Class = rep('character', 3),
#'     Length = c('11', '10', '10'),
#'     Qual1 = c(rep('', 2), 'NumIdEst'),
#'     Qual2 = c(rep('', 2), 'Action'),
#'     ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation', 
#'                     '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )')))
#'                     
#' Aggdt <- new(Class='DDdt',
#'   data.table(
#'     Variable = c('CCAA', 'NACE09', 'Ponderacion'),
#'     Sort = c(rep('IDQual', 2), 'IDDD'),
#'     Class = c(rep('character', 2), 'numeric'),
#'     Length = c('2', '4', '7'),
#'     Qual1 = c(rep('', 2), 'CCAA'),
#'     Qual2 = c(rep('', 2), 'NACE09'),
#'     ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )')))
#' 
#' DD <- new(Class = 'DD', 
#'           VarNameCorresp = VNC, 
#'           ID = IDdt, 
#'           MicroData = Microdt, 
#'           ParaData = Paradt,
#'           AggWeights = Aggdt)
#' 
#' getParaData(DD)
#' 
#' StQ <- new(Class = 'StQ', Data = new(Class = 'Datadt'), DD = DD)
#' getOtherDD(StQ)
#' 
#' rawStQ <- new(Class = 'rawStQ', Data = new(Class = 'rawDatadt'), DD = DD)
#' getOtherDD(rawStQ)

>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' 
#' @export
setGeneric("getParaData", function(object){standardGeneric("getParaData")})

#' @rdname getParaData
#' 
<<<<<<< HEAD
#' @export
setMethod(
    f = "getParaData",
    signature = c("VNC"),
    function(object){object[['ParaData']]}
=======
#' @include DD-class.R
#' 
#' @export
setMethod(
    f = "getParaData",
    signature = c("DD"),
    function(object){
        
        out <- copy(object@ParaData)
        return(out)
    }
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
)

#' @rdname getParaData
#' 
<<<<<<< HEAD
#' @export
setMethod(
    f = "getParaData",
    signature = c("DD"),
    function(object){object[['ParaData']]}
)
=======
#' @include StQ-class.R getDD.R
#' 
#' @export
setMethod(
    f = "getParaData",
    signature = c("StQ"),
    function(object){
        
        output <- getParaData(getDD(object))
        return(output)
    }
)

#' @rdname getParaData
#' 
#' @include rawStQ-class.R getDD.R
#' 
#' @export
setMethod(
    f = "getParaData",
    signature = c("rawStQ"),
    function(object){
        
        output <- getParaData(getDD(object))
        return(output)
    }
)

>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8