#' @title Generates an object of class \linkS4class{StQ} from the input
#' \linkS4class{rawStQ} object.
#'
#' @description \code{rawStQToStQ} returns an object of class \linkS4class{StQ}
#' from the input\linkS4class{rawStQ} object. 
#' 
#' @param rawQ Object of class \linkS4class{rawStQ} whose slot Data has the
#' key-value pair structure.
#'
#' @return Object of class \linkS4class{StQ} whose slot Data has at least the 
#' columns \code{IDDD} and \code{Value}.
#'
#' @examples
#' data(ExampleDD)
#' key <- new(Class = 'rawKey', c('IDDD:Turnover_NumIdEst:001_IsNatMarket:0', 'IDDD:Turnover_NumIdEst:001_IsNatMarket:1', 'IDDD:Turnover_NumIdEst:001_Province:04', 'IDDD:Turnover_NumIdEst:001_NACE09:0512'))
#' rawData <- new(Class = 'rawDatadt', data.table(Key = key, Value = c('625000', '23154', '25004', '10512')))
#' rawQ <- new(Class = 'rawStQ', Data = Data, DD = ExampleDD)
#' 
#' newQ <- rawStQToStQ(rawQ)
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
        
        rawDT <- KeyToDT(getData(rawQ)[['Key']])
        rawDT <- rawDT[, Value := getData(rawQ)[['Value']]]
        setcolorder(rawDT, c(setdiff(names(rawDT), c('IDDD', 'Value')), c('IDDD', 'Value')))
        
        rawDatadt <- new(Class = 'Datadt', rawDT)
        Q <- new(Class = 'StQ', Data = rawDatadt, DD = rawDD)
        
        return(Q)
    }
)
