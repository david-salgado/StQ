#' @title Return the variable names included in columns 'NonIDQual" of the input object
#'
#' @description \code{getNonIDQual} returns a character vector with all unit qualifier 
#' names (NonIDQual) included in the input object.
#'
#' @param object Object with the NonIDQual unit qualifier. 
#' 
#' @param Namesdt Character vector with the components or slots from which 
#' NonIDQuals are requiered. 
#'
#' @return Character vector with all the variable qualifier names.
#'
#' @examples
#' # A more elaborate example
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
#'  Aggregates = new(Class = 'VNCdt', 
#'                   .Data = data.table(
#'                      IDQual = c('Province', 'NACE', 'IsNatMarket', ''),
#'                      NonIDQual = rep('', 4),
#'                      IDDD = c('', '', '', 'TotalTurnover'),
#'                      Province = c('', '', '', '.'),
#'                      NACE = c('', '', '', '.'),
#'                      IsNatMarket = c('', '', '', '1'),
#'                      Unit1 = c('provincia', 'actividad', '', 'cn01'))))
#' Example <- new(Class = 'VarNameCorresp', .Data = VarList)
#' getNonIDQual(Example)
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
    function(object){
        
        output <- unique(object@Data[['NonIDQual']])
        return(output)
    }
)
