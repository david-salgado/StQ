#' @title Return the variable names included in columns 'NonIDQual" of the input object
#'
#' @description \code{getNonIDQual} returns a character vector with all variable 
#' qualifiers names (NonIDQual) included in the input object.
#'
#' @param object Object with the NonIDQual variable qualifiers. 
#' 
#' @param Namesdt Character vector with the components or slots of the object
#' from which NonIDQuals are required. 
#'
#' @return Character vector with the NonIDQual variable qualifiers names.
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
#' 
#' @export
setGeneric("getNonIDQual", function(object, Namesdt){standardGeneric("getNonIDQual")})

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
    function(object, Namesdt){
        
        if (missing(Namesdt)) Namesdt <- names(object)
        
        aux <- object[Namesdt]
        
        NonIDQual.list <- lapply(aux, function(x) { 
            NonIDQual <- getNonIDQual(x)
            return(NonIDQual)
        }
        )
        
        output <- unique(Reduce(c, NonIDQual.list, init = NonIDQual.list[[1]]))
        return(output)
        
    }
)

#' @rdname getNonIDQual
#' 
#' @include DDdt-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("DDdt"),
    function(object){
        
        output <- unique(object[Sort == 'NonIDQual', Variable])
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
    function(object, Namesdt){
        
        if (missing(Namesdt)) Namesdt <- slotNames(object)
        
        Namesdt <- setdiff(Namesdt, 'VarNameCorresp')
        output <- c()
        for (DDdt in Namesdt) {
            
            NonIDQual <- getNonIDQual(slot(object,DDdt))
            output <- c(output, NonIDQual)
        }
        
        output <- unique(output)
        return(output)
    }
)

#' @rdname getNonIDQual
#' 
#' @include StQ-class.R 
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getNonIDQual",
    signature = c("StQ"),
    function(object, Namesdt){
        
        if (missing(Namesdt)) {output <- getNonIDQual(object@DD)
        } else {output <- getNonIDQual(object@DD, Namesdt)}
        
        output <- intersect(names(object@Data), output)
        return(output)
    }
)
