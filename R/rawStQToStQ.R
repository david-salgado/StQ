#' @title Generates an object of class \linkS4class{StQ} from the input \linkS4class{rawStQ} object.
#'
#' @description \code{rawStQToStQ} returns an object of class \linkS4class{StQ} from the input
#' \linkS4class{rawStQ} object. 
#' 
#' @param rawQ Object of class \linkS4class{rawStQ} whose slot Data has the  key-value pair
#' structure.
#'
#' @return Object of class \linkS4class{StQ} whose slot Data has at least the columns \code{IDDD}
#' and \code{Value}.
#'
#' @examples
#' data(ExamplerawStQ)
#' StQ <- rawStQToStQ(ExamplerawStQ)
#' str(StQ)
#' 
#' @export
setGeneric("rawStQToStQ", function(rawQ){standardGeneric("rawStQToStQ")})
#' @rdname rawStQToStQ
#' 
#' @include rawStQ-class.R Datadt-class.R StQ-class.R
#' 
#' @export
setMethod(
    f = "rawStQToStQ",
    signature = c("rawStQ"),
    function(rawQ){
        
        rawDD <- getDD(rawQ)
        
        rawDT <- KeyToDT(getData(rawQ)[['Key']], getDD(rawQ))
        rawDT <- rawDT[, Value := getData(rawQ)[['Value']]]
        setcolorder(rawDT, c(setdiff(names(rawDT), c('IDDD', 'Value')), c('IDDD', 'Value')))
        
        rawDatadt <- new(Class = 'Datadt', rawDT)
        Q <- new(Class = 'StQ', Data = rawDatadt, DD = rawDD)
        
        return(Q)
    }
)
