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
#' data(ExampleStQ)
#' newrawQ <- StQTorawStQ(ExampleStQ)
#'  
#' @export
setGeneric("StQTorawStQ", function(Q){standardGeneric("StQTorawStQ")})
#' @rdname StQTorawStQ
#' 
#' @include StQ-class.R rawKey-class.R rawDatadt-class.R rawStQ-class.R getDD.R getData.R DTToKey.R
#' 
#' @export
setMethod(
    f = "StQTorawStQ",
    signature = c("StQ"),
    function(Q){
        
        QDD <- getDD(Q)
        
<<<<<<< HEAD
        QData <- getData(Q)
        #nsep <- length(names(QData))
        QData <- DTToKey(QData)
        value <- unlist(lapply(strsplit(QData, '@@'), function(x){x[length(x)]}))
        key <-  lapply(strsplit(QData, '@@'), function(x){x[1:(length(x) - 1)]})
        #value <- unlist(lapply(StrSplit(QData@.Data, nsep, '@@'), function(y){y[length(y)]}))
        #key <-  lapply(StrSplit(QData@.Data, nsep, '@@'), function(y){y[1:(length(y) - 1)]})
        key <- lapply(key, paste0, '@@')
        key <- unlist(lapply(key, function(x){Reduce(paste0, x)}))
||||||| merged common ancestors
        QData <- getData(Q)
        QData <- DTToKey(QData)
        value <- unlist(lapply(strsplit(QData, '@@'), function(x){x[length(x)]}))
        key <-  lapply(strsplit(QData, '@@'), function(x){x[1:(length(x) - 1)]})
        key <- lapply(key, paste0, '@@')
        key <- unlist(lapply(key, function(x){Reduce(paste0, x)}))
=======
        QData <- DatadtToDT(getData(Q))
        Value <- QData[['Value']]
        QData[, Value := NULL]
        key <- DTToKey(QData)
>>>>>>> ff277f6cdd5fddd61ed2d53b488ca2b0d15d8b1f
        key <- new(Class = 'rawKey', key)    
        rawDatadt <- new(Class = 'rawDatadt', data.table(Key = key, Value = Value))
        rawQ <- new(Class = 'rawStQ', Data = rawDatadt, DD = QDD)
        return(rawQ)
    }
)
