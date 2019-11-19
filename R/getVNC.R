#' @title Return slot \code{VNC} from an object
#'
#' @description \code{getVNC} extracts slot \code{VarNameCorresp} from an object of class 
#' \link{DD} or class \link{StQ}.
#'
#' This method returns an object of class \linkS4class{VarNameCorresp} from an object of class 
#' \link{DD} or \link{StQ} specified as input argument.
#'
#' @param object Object of class \link{DD} or \link{StQ}.
#'
#' @return Object of class \linkS4class{VarNameCorresp} of the input object.
#'
#' @examples 
#' library(data.table)
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
#'                        Market = c(rep('', 2), '1'),
#'                        UnitName = c('numidest', '', 'cp09'),
#'                        InFiles = rep('FF, FD, FG', 3)),
#' ParaData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                       NonIDQual = c('', 'Action', ''),
#'                       IDDD = c(rep('', 2), 'Date'),
#'                       NumIdEst = c(rep('', 2), '.'),
#'                       Action = c(rep('', 2), 'Imputation'),
#'                       UnitName = c('numidest', '', 'FechaImput'),
#'                       InFiles = rep('FP', 3)))
#'
#' VNC <- BuildVNC(VarList)
#'
#' IDdt <- data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                    Sort = c('IDQual', rep('IDDD', 4)),
#'                    Class = rep('character', 5),
#'                    Length = c('11', '15', '15', '20','9'),
#'                    Qual1 = c('', rep('NumIdEst', 4)),
#'                    ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}'))
#' Microdt <- data.table(Variable = c('NumIdEst', 'Market', 'NewOrders'),
#'                       Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                       Class = c(rep('character', 2), 'numeric'),
#'                       Length = c('11', '2', '7'),
#'                       Qual1 = c(rep('', 2), 'NumIdEst'),
#'                       ValueRegExp = c('[0-9]{9}PP', '.+', '([0-9]{1, 10}| )'))
#' Paradt <- data.table(Variable = c('NumIdEst', 'Action', 'Date'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = rep('character', 3),
#'                      Length = c('11', '10', '10'),
#'                      Qual1 = c(rep('', 2), 'NumIdEst'),
#'                      Qual2 = c(rep('', 2), 'Action'),
#'                      ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation',
#'                                      '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )'))
#'
#' DD <- DD(VNC = VNC, ID = IDdt, MicroData = Microdt, ParaData = Paradt)
#' getVNC(DD)
#' 
#' @import data.table
#' 
#' @include DD.R VNC.R BuildVNC.R BuildDD.R getDD.R
#'  
#' @export
setGeneric("getVNC", function(object) {standardGeneric("getVNC")})

#' @rdname getVNC
#'
#' @export
setMethod(
    f = "getVNC",
    signature = c("DD"),
    function(object){object$VNC}
)
#' @rdname getVNC
#'
#' @export
setMethod(
    f = "getVNC",
    signature = c("StQ"),
    function(object){
        
        VNC <- getVNC(getDD(object))
        return(VNC)
        
    }
)

#' @rdname getVNC
#'
#' @export
setMethod(
    f = "getVNC",
    signature = c("rawStQ"),
    function(object){
        
        VNC <- getVNC(getDD(object))
        return(VNC)
        
    }
)
