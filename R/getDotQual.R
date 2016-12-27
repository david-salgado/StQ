#' @title Return IDDD identifiers from an object
#'
#' @description \code{getDotQual} returns a character vector with all qualifier names (IDQual) 
#' with wildcard dot in the VNC component the input object.
#'
#' @param object Object with the dot qualifiers.
#'
#' @param Namesdt Character vector with the components or slots from which dot qualifiers are 
#' required.
#'
#' @return Character vector with all the dot qualifier names.
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
#' getDotQual(VNC)
#'
#'
#' @export
setGeneric("getDotQual", function(object, Namesdt){standardGeneric("getDotQual")})

#' @rdname getDotQual
#'
#' @include VNCdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getDotQual",
    signature = c("VNCdt"),
    function(object, Namesdt){
        
        ColNames <- names(object)
        output <- c()
        for (col in ColNames){
            
            if (any(object[[col]] == '.')) output <- c(output, col)
            
        }
        return(output)
        
    }
)
#' @rdname getDotQual
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getDotQual",
    signature = c("VarNameCorresp"),
    function(object, Namesdt){
        
        if (missing(Namesdt)) Namesdt <- names(object)
        
        aux <- object[Namesdt]
        
        IDQual.list <- lapply(aux, function(x) {
            IDQual <- getDotQual(x)
            return(IDQual)
        }
        )
        
        output <- unique(Reduce(c, IDQual.list, init = IDQual.list[[1]]))
        return(output)
        
    }
)


#' @rdname getDotQual
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getDotQual",
    signature = c("DD"),
    function(object, Namesdt){
        
        VNC <- getVNC(object)
        output <- getDotQual(VNC, Namesdt)
        return(output)
    }
)

#' @rdname getDotQual
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getDotQual",
    signature = c("StQ"),
    function(object, Namesdt){
        
        if (missing(Namesdt)) {
            
            output <- getDotQual(getDD(object))
        
        } else{
            
            output <- getDotQual(getDD(object), Namesdt)
        
        }
        
        return(output)
    }
)

