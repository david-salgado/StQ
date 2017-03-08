<<<<<<< HEAD
#' @title Return component ID from an object
=======
#' @title Return slot ID from an object
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @description \code{getID} returns slot \code{ID} of the input object.
#' 
#' @param object An object whose slot \code{ID} is queried.
#'
<<<<<<< HEAD
#' @return Returns a \linkS4class{data.table} containing the slot \code{ID} of the input object.
=======
#' @return Returns a \linkS4class{DDdt} object containing the slot \code{ID} of the input object.
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
#' 
#' getID(VNC)
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
#'                                            InFiles = rep('FF', 3))),
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
#'                                            InFiles = rep('FF', 3))))
#' 
#' VNC <- new(Class = 'VarNameCorresp', VarList)
#' 
#' ### We build the specification data.tables
#' IDdt <- new( Class='DDdt', data.table(
#'     Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'     Sort = c('IDQual', rep('IDDD', 4)),
#'     Class = rep('character', 5),
#'     Length = c('11', '15', '15', '20','9'),
#'     Qual1 = c('', rep('NumIdEst', 4)),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')))
#' Microdt <- new( Class='DDdt', data.table(
#'     Variable = c('NumIdEst', 'Market', 'NewOrders'),
#'     Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'     Class = c(rep('character', 2), 'numeric'),
#'     Length = c('11', '2', '7'),
#'     Qual1 = c(rep('', 2), 'NumIdEst'),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '([0-9]{1, 10}| )')))
#' Paradt <-new( Class='DDdt', data.table(
#'     Variable = c('NumIdEst', 'Action', 'Date'),
#'     Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'     Class = rep('character', 3),
#'     Length = c('11', '10', '10'),
#'     Qual1 = c(rep('', 2), 'NumIdEst'),
#'     Qual2 = c(rep('', 2), 'Action'),
#'     ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation', 
#'                     '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )')))
#' Aggdt <- new(Class='DDdt',
#'              data.table(Variable = c('CCAA', 'NACE09', 'Ponderacion'),
#'                         Sort = c(rep('IDQual', 2), 'IDDD'),
#'                         Class = c(rep('character', 2), 'numeric'),
#'                         Length = c('2', '4', '7'),
#'                         Qual1 = c(rep('', 2), 'CCAA'),
#'                         Qual2 = c(rep('', 2), 'NACE09'),
#'                         ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', 
#'                                         '([0-9]{1, 15}| )')))
#' 
#' DD <- new(Class = 'DD', 
#'           VarNameCorresp = VNC, 
#'           ID = IDdt, 
#'           MicroData = Microdt, 
#'           ParaData = Paradt,
#'           AggWeights = Aggdt)
#'           
#' getID(DD)
#' 
#' StQ <- new(Class = 'StQ', Data = new(Class = 'Datadt'), DD = DD)
#' getID(StQ)
#' 
#' rawStQ <- new(Class = 'rawStQ', Data = new(Class = 'rawDatadt'), DD = DD)
#' getID(rawStQ)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' 
#' @export
setGeneric("getID", function(object){standardGeneric("getID")})

#' @rdname getID
#' 
<<<<<<< HEAD
=======
#' @include DD-class.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' 
#' @export
setMethod(
    f = "getID",
<<<<<<< HEAD
    signature = c("VNC"),
    function(object){object[['ID']]}
=======
    signature = c("DD"),
    function(object){
        
        out <- copy(object@ID)
        return(out)
    }
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
)

#' @rdname getID
#' 
<<<<<<< HEAD
=======
#' @include StQ-class.R getDD.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' 
#' @export
setMethod(
    f = "getID",
<<<<<<< HEAD
    signature = c("DD"),
    function(object){object[['ID']]}
)

=======
    signature = c("StQ"),
    function(object){
        
        DD <- getDD(object)
        output <- getID(DD)
        return(output)
    }
)

#' @rdname getID
#' 
#' @include rawStQ-class.R getDD.R
#' 
#' @export
setMethod(
    f = "getID",
    signature = c("rawStQ"),
    function(object){
        
        DD <- getDD(object)
        output <- getID(DD)
        return(output)
    }
)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8