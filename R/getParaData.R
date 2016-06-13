#' @title Return slot ParaData from a DD object
#'
#' @description \code{getParaData} returns slot \code{ParaData} of the input
#' \linkS4class{DD} object.
#' 
#' @param object a DD Object whose slot \code{ParaData} is queried.
#'
#' @return \linkS4class{data.table} with data from slot \code{ParaData} of the
#' input \linkS4class{DD} object.
#'
#' @examples
#' # An example:
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = new(Class = 'VNCdt',
#'                          data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                     NonIDQual = c('','','','',''),
#'                                     IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                     NumIdEst = c('', rep('.', 4)),
#'                                     Unit1 = c('numidest', 'nombre', 'apellidos', 'direccion', 
#'                                               'telefono'))),
#'                 MicroData = new(Class = 'VNCdt', 
#'                                 data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                            NonIDQual = c('', 'IsNatMarket', 'IsEuroMarket', 
#'                                                          'IsRWMarket', ''),
#'                                            IDDD = c(rep('', 4), 'NewOrders'),
#'                                            NumIdEst = c(rep('', 4), '.'),
#'                                            IsNatMarket = c(rep('', 4), '0'),
#'                                            IsEuroMarket = c(rep('', 4), '0'),
#'                                            IsRWMarket = c(rep('', 4), '1'),
#'                                            Unit1 = c('numidest', rep('', 3), 'cp09'))),
#'                ParaData = new(Class = 'VNCdt',
#'                               data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                          NonIDQual = c('', 'Action', ''),
#'                                          IDDD = c(rep('', 2), 'Date'),
#'                                          NumIdEst = c(rep('', 2), '.'),
#'                                          Action = c(rep('', 2), 'Imputation'),
#'                                          Unit1 = c('numidest', '', 'FechaImput'))),
#'                AggWeights = new(Class = 'VNCdt',
#'                                 data.table(IDQual = c('CCAA', 'NACE09', ''),
#'                                            NonIDQual = rep('', 3),
#'                                            IDDD = c('', '', 'Ponderacion'),
#'                                            CCAA = c('', '', '.'),
#'                                            NACE09 = c('', '', '.'),
#'                                            Unit1 = c('Provincia', '', ''))))
#' 
#' VNC <- new(Class = 'VarNameCorresp', VarList)
#' 
#' ### We build the specification data.tables
#' IDdt <- new(Class='DDdt',
#'             data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                        Sort = c('IDQual', rep('IDDD', 4)),
#'                        Class = rep('character', 5),
#'                        QualOrder = c('1', rep('', 4)),
#'                        Qual1 = c('', rep('NumIdEst', 4)),
#'                        ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')))
#' Microdt <- new(Class='DDdt',
#'                data.table(Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 'IsRWMarket', 
#'                                        'NewOrders'),
#'                           Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'                           Class = c(rep('character', 4), 'numeric'),
#'                           QualOrder = c('1', '2', '3', '4', ''),
#'                           Qual1 = c(rep('', 4), 'NumIdEst'),
#'                           ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), '([0-9]{1, 10}| )')))
#' Paradt <- new(Class='DDdt', 
#'               data.table(Variable = c('NumIdEst', 'Action', 'Date'),
#'                          Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                          Class = rep('character', 3),
#'                          QualOrder = c('1', '5', ''),
#'                          Qual1 = c(rep('', 2), 'NumIdEst'),
#'                          Qual2 = c(rep('', 2), 'Action'),
#'                          ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation', 
#'                                          '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )')))
#' Aggdt <- new(Class='DDdt',
#'              data.table(Variable = c('CCAA', 'NACE09', 'Ponderacion'),
#'                         Sort = c(rep('IDQual', 2), 'IDDD'),
#'                         Class = c(rep('character', 2), 'numeric'),
#'                         QualOrder = c('6', '7', ''),
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
#' getParaData(DD)
#' 
#' @export
setGeneric("getParaData", function(object){standardGeneric("getParaData")})
#' @rdname getParaData
#' 
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
)

