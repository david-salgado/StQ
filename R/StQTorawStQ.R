#' @title Generates an object of class \linkS4class{rawStQ} from the input
#' \linkS4class{StQ} object.
#'
#' @description \code{StQTorawStQ} returns an object of class
#' \linkS4class{rawStQ} from the input\linkS4class{StQ} object. 
#' 
#' @param Q Object of class \linkS4class{StQ} whose slot Data has at least the 
#' columns \code{IDDD} and \code{Value}.
#'
#' @return Object of class \linkS4class{rawStQ} whose slot Data has the
#' key-value pair structure.
#'
#' @examples
#' library(data.table)
#' data(Q)
#' newrawQ <- StQTorawStQ(Q)
#'  
#' @export
setGeneric("StQTorawStQ", function(Q){standardGeneric("StQTorawStQ")})
#' @rdname StQTorawStQ
#' 
#' @include StQ-class.R rawKey-class.R rawDatadt-class.R rawStQ-class.R 
#' 
#' @export
setMethod(
    f = "StQTorawStQ",
    signature = c("StQ"),
    function(Q){
        
        QDD <- getDD(Q)
        
        QData <- getData(Q)
        QData <- DTToKey(QData)
        keyValue <- strsplit(QData, '_Value:')
        key <- unlist(lapply(keyValue, function(x){x[1]}))
        value <- unlist(lapply(keyValue, function(x){x[2]}))
        
        key <- new(Class = 'rawKey', key)    
        Datadt <- new(Class = 'rawDatadt', data.table(Key = key, Value = value))
        rawQ <- new(Class = 'rawStQ', Data = Datadt, DD = QDD)

        return(rawQ)
    }
)
