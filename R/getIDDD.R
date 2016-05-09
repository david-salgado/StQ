#' @title Return IDDD identifiers from an object
#'
#' @description \code{getIDDD} returns the IDDD identifiers from the input 
#' object.  
#' @param object Object whose IDDD identifiers are required.
#' 
#' @param CompNames Character vector with the names of components or slots of the 
#' object from which the IDDD identifiers are requested.
#'
#' @return In the case of \linkS4class{VarNameCorresp} it returns a character
#' vector with the IDDD identifiers from the components specified in Namesdt. 
#' If no Namesdt is specified, it returns the IDDD identifier from all the 
#' components of the object.
#'
#' @examples
#' library(data.table)
#' VarList <- list(
#'   ID = new(Class = 'VNCdt', 
#'            .Data = data.table(
#'                  IDQual = c('NumIdEst', rep('', 4)),
#'                  NonIDQual = rep('', 5),
#'                  IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                  NumIdEst = c('', rep('.', 4)),
#'                  Unit1 = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'))),
#'   MicroData = new(Class = 'VNCdt', 
#'                   .Data = data.table(
#'                       IDQual = c('NumIdEst', rep('', 4)),
#'                       NonIDQual = c('', 'IsNatMarket', 'IsEuroMarket', 'IsRWMarket', ''),
#'                       IDDD = c(rep('', 4), 'NewOrders'),
#'                       NumIdEst = c(rep('', 4), '.'),
#'                       IsNatMarket = c(rep('', 4), '0'),
#'                       IsEuroMarket = c(rep('', 4), '0'),
#'                       IsRWMarket = c(rep('', 4), '1'),
#'                       Unit1 = c('numidest', rep('', 3), 'cp09'))),
#'   ParaData = new(Class = 'VNCdt'),
#'   Aggregates = new(Class = 'VNCdt', 
#'                    .Data = data.table(
#'                       IDQual = c('Province', 'NACE', 'IsNatMarket', ''),
#'                       NonIDQual = rep('', 4),
#'                       IDDD = c('', '', '', 'TotalTurnover'),
#'                       Province = c('', '', '', '.'),
#'                       NACE = c('', '', '', '.'),
#'                       IsNatMarket = c('', '', '', '1'),
#'                       Unit1 = c('provincia', 'actividad', '', 'cn01'))))
#' VNC <- new(Class = 'VarNameCorresp', .Data = VarList)
#' getIDDD(VNC)
#' 
#' 
#' @export
setGeneric("getIDDD", function(object, CompNames){standardGeneric("getIDDD")})

#' @rdname getIDDD
#' 
#' @include VNCdt-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("VNCdt"),
    function(object, CompNames){
        
        output <- unique(object[['IDDD']])
        output <- output[output != '']
        return(output)
        
    }
)
#' @rdname getIDDD
#' 
#' @include VarNameCorresp-class.R 
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("VarNameCorresp"),
    function(object, CompNames){
        
        if (missing(CompNames)) CompNames <- names(object)
        
        aux <- object[CompNames]
        
        IDDD.list <- lapply(aux, function(x) { 
            IDDD <- getIDDD(x)
            return(IDDD)
        }
        )
        
        output <- unique(Reduce(c, IDDD.list, init = IDDD.list[[1]]))
        return(output)
        
    }
)

#' @rdname getIDDD
#' 
#' @include DDdt-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("DDdt"),
    function(object, CompNames){
        
        output <- unique(object[Sort == 'IDDD', Variable])
        output <- output[output != '']
        
        return(output)
    }
)

#' @rdname getIDDD
#' 
#' @include DD-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("DD"),
    function(object, CompNames){
        
        if (missing(CompNames)) CompNames <- slotNames(object)
        
        output <- c()
        for (slotDD in CompNames) {
            
            IDDD <- getIDDD(slot(object,slotDD))
            output <- c(output, IDDD)
        }
        
        output <- unique(output)
        return(output)
    }
)

#' @rdname getIDDD
#' 
#' @include Datadt-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("Datadt"),
    function(object, CompNames){
        
        output <- unique(object[['IDDD']])
        return(output)
    }
)

#' @rdname getIDDD
#' 
#' @include StQ-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("StQ"),
    function(object, CompNames){
        
        output <- getIDDD(object@Data)
        return(output)
    }
)

