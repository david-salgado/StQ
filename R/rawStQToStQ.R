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
<<<<<<< HEAD
#' library(data.table)
#' data(ExampleDD)
#' key <- new(Class = 'rawKey', 
#'            c('Turnover@@001@@@@', 
#'              'Employees@@001@@1@@0', 
#'              'Employees@@001@@0@@', 
#'              'Employees@@001@@1@@1'))
#' rawData <- new(Class = 'rawDatadt', 
#'                data.table(Key = key, Value = c('625000', '23154', '25004', '10512')))
#' rawQ <- new(Class = 'rawStQ', Data = rawData, DD = ExampleDD)
||||||| merged common ancestors
#' library(data.table)
#' data(ExampleDD)
#' key <- new(Class = 'rawKey', 
#'            c('IDDD:Turnover_ID:001', 
#'              'IDDD:Employees_ID:001_IsRemun:1_IsPartTime:0', 
#'              'IDDD:Employees_ID:001_IsRemun:0', 
#'              'IDDD:Employees_ID:001_IsRemun:1_IsPartTime:1'))
#' rawData <- new(Class = 'rawDatadt', 
#'                data.table(Key = key, Value = c('625000', '23154', '25004', '10512')))
#' rawQ <- new(Class = 'rawStQ', Data = rawData, DD = ExampleDD)
=======
#' data(ExamplerawStQ)
#' StQ <- rawStQToStQ(ExamplerawStQ)
#' str(StQ)
>>>>>>> ff277f6cdd5fddd61ed2d53b488ca2b0d15d8b1f
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
        
        DDslotNames <- setdiff(slotNames(object@DD), 'VarNameCorresp')
        rawDD <- Reduce()

        #nQuals <- dim(rawDT)[2] - 2
        #Quals <- paste0('Qual', seq(1:nQuals))
        names(rawDT) <- c('IDDD', Quals, 'Value')
        setcolorder(rawDT, c(Quals, 'IDDD', 'Value'))
        #setcolorder(rawDT, c(setdiff(names(rawDT), c('IDDD', 'Value')), c('IDDD', 'Value')))
        
        rawDatadt <- new(Class = 'Datadt', rawDT)
        Q <- new(Class = 'StQ', Data = rawDatadt, DD = rawDD)
        
        return(Q)
    }
)
