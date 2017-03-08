#' @title Return non-unit qualifiers used in compound variable names
#'
#' @description \code{getDoubleDotQual} returns a character vector with all non-unit qualifier names 
#' specified with a double wildcard dot in the VNC component of the input object.
#'
#' @param object Object containing the dot qualifiers.
#'
#' @param Component \code{Character} vector with the components or slots from which dot qualifiers 
#' are required.
#'
#' @return \code{Character} vector with all the double dot qualifier names.
#'
#' @examples
#' library(data.table)
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
#'                        InFiles = rep('FF, FD, FG', 3)),
#' ParaData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                       NonIDQual = c('', 'Action', ''),
#'                       IDDD = c(rep('', 2), 'Date'),
#'                       NumIdEst = c(rep('', 2), '.'),
#'                       Action = c(rep('', 2), '..'),
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
#' getDoubleDotQual(VNC)
#'
#' @include VNC.R DD.R StQ.R getVNC.R getDD.R
=======
#' VarList <- list(ID = new(Class = 'VNCdt',
#'                 data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                            NonIDQual = c('','','','',''),
#'                            IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                            NumIdEst = c('', rep('.', 4)),
#'                            UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                            InFiles = rep('FI', 5))),
#' MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                             NonIDQual = c('', 'Market', ''),
#'                                             IDDD = c(rep('', 2), 'NewOrders'),
#'                                             NumIdEst = c(rep('', 2), '.'),
#'                                             Market = c(rep('', 2), '1.'),
#'                                             UnitName = c('numidest', '', 'cp09'),
#'                                             InFiles = rep('FF, FD, FG', 3))),
#' ParaData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                            NonIDQual = c('', 'Action', ''),
#'                                            IDDD = c(rep('', 2), 'Date'),
#'                                            NumIdEst = c(rep('', 2), '.'),
#'                                            Action = c(rep('', 2), '..'),
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
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @export
setGeneric("getDoubleDotQual", 
           function(object, Component = names(object)){standardGeneric("getDoubleDotQual")})

#' @rdname getDoubleDotQual
#'
<<<<<<< HEAD
=======
#' @include VNCdt-class.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @import data.table
#'
#' @export
setMethod(
    f = "getDoubleDotQual",
<<<<<<< HEAD
    signature = c("VNC"),
=======
    signature = c("VNCdt"),
    function(object, Component = names(object)){
        
        ColNames <- names(object)
        output <- c()
        for (col in ColNames){
            
            if (any(object[[col]] == '..')) output <- c(output, col)
            
        }
        return(output)
        
    }
)
#' @rdname getDoubleDotQual
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getDoubleDotQual",
    signature = c("VarNameCorresp"),
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
    function(object, Component = names(object)){
        
        ValidComp <- names(object)
        NotValidComp <- Component[!Component %in% ValidComp]
<<<<<<< HEAD
        if (!all(Component %in% ValidComp)) stop(paste0('[StQ::getDotQual] The following components are not present in the input object: ', 
                                                       paste0(NotValidComp, collapse = ', '), '.\n'))
        aux <- lapply(Component, function(Comp){object[[Comp]]})
        DoubleDotQual.list <- lapply(aux, function(DT){
            
            ColNames <- names(DT)
            LocalOutput <- c()
            for (col in ColNames){
                
                if (any(DT[[col]] == '..')) LocalOutput <- c(LocalOutput, col)
                
            }
            return(LocalOutput)  
        })
        output <- unique(Reduce(c, DoubleDotQual.list, init = DoubleDotQual.list[[1]]))
=======
        if(!all(Component %in% ValidComp)) stop(paste0('[StQ::getDotQual] The following components are not present in the input object: ', 
                                                       paste0(NotValidComp, collapse = ', '), '.\n'))
        aux <- object[Component]
        IDQual.list <- lapply(aux, function(x){getDoubleDotQual(x, Component)})
        output <- unique(Reduce(c, IDQual.list, init = IDQual.list[[1]]))
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(output)
        
    }
)


#' @rdname getDoubleDotQual
#'
<<<<<<< HEAD
=======
#' @include DD-class.R getVNC.R
#'
#' @import data.table
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @export
setMethod(
    f = "getDoubleDotQual",
    signature = c("DD"),
    function(object, Component = names(getVNC(object))){
        
        VNC <- getVNC(object)
        output <- getDoubleDotQual(VNC, Component)
        return(output)
    }
)
<<<<<<< HEAD
=======

#' @rdname getDoubleDotQual
#'
#' @include StQ-class.R getDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getDoubleDotQual",
    signature = c("StQ"),
    function(object, Component){
        
        if (missing(Component)){
            
            output <- getDoubleDotQual(getDD(object))
            
        } else {
            
            output <- getDoubleDotQual(getDD(object), Component)
            
        }
        
        return(output)
    }
)

>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
