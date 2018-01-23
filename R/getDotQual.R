#' @title Return non-unit qualifiers not used in compound variable names
#'
#' @description \code{getDotQual} returns a character vector with all qualifier names specified with
#' a wildcard dot in the VNC component of the input object.
#'
#' @param object Object containing the dot qualifiers.
#'
#' @param Component \code{Character} vector with the components or slots from which dot qualifiers 
#' are required.
#'
#' @return \code{Character} vector with all the dot qualifier names.
#'
#' @examples
#' library(data.table)
#' VarList <- list(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                 NonIDQual = c('','','','',''),
#'                                 IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                 NumIdEst = c('', rep('.', 4)),
#'                                 UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 
#'                                              'telefono'),
#'                                 InFiles = rep('FI', 5)),
#' MicroData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                        NonIDQual = c('', 'Market', ''),
#'                        IDDD = c(rep('', 2), 'NewOrders'),
#'                        NumIdEst = c(rep('', 2), '.'),
#'                        Market = c(rep('', 2), '1.'),
#'                        UnitName = c('numidest', '', 'cp09'),
#'                        InFiles = rep('FF, FD, FG', 3)),
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
#'                         InFiles = rep('FA', 3)))
#' VNC <- BuildVNC(VarList)
#' getDotQual(VNC)
#'
#' @include VNC.R DD.R StQ.R getVNC.R getDD.R
#'
#' @import data.table
#'   
#' @export
setGeneric("getDotQual", function(object, Component = names(object)){standardGeneric("getDotQual")})

#' @rdname getDotQual
#'
#' @export
setMethod(
    f = "getDotQual",
    signature = c("VNC"),
    function(object, Component = names(object)){

        ValidComp <- names(object)
        NotValidComp <- Component[!Component %in% ValidComp]
        if (!all(Component %in% ValidComp)) stop(paste0('[StQ::getDotQual] The following components are not present in the input object: ', 
                                                       paste0(NotValidComp, collapse = ', '), '.\n'))
        aux <- lapply(Component, function(Comp){object[[Comp]]})
        DotQual.list <- lapply(aux, function(DT){
            
            ColNames <- names(DT)
            LocalOutput <- c()
            for (col in ColNames){
                
                if (any(DT[[col]] == '.')) LocalOutput <- c(LocalOutput, col)
                
            }
            return(LocalOutput)
        })
        output <- unique(Reduce(c, DotQual.list, init = DotQual.list[[1]]))
        return(output)
        
    }
)


#' @rdname getDotQual
#'
#' @export
setMethod(
    f = "getDotQual",
    signature = c("DD"),
    function(object, Component = names(getVNC(object))){
        
        VNC <- getVNC(object)
        output <- getDotQual(VNC, Component = Component)
        return(output)
    }
)


#' @rdname getDotQual
#'
#' @export
setMethod(
    f = "getDotQual",
    signature = c("StQ"),
    function(object, Component = names(getVNC(object))){
        
        VNC <- getVNC(object)
        output <- getDotQual(VNC, Component = Component)
        return(output)
    }
)
