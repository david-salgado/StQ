#' @title Return a \linkS4class{data.table} out of a set of parsed keys
#'
#' @description \code{KeyToDT} returns a \linkS4class{data.table} whose columns
#' are the different qualifiers parsed in the input key with as many raws as
#' components (length) of this input key. 
#' 
#' @param key key implemented either through the class \linkS4class{rawKey} or 
#' other.
#'
#' @return \linkS4class{data.table} with one column per each different qualifier
#' found in the input key and as many rows as components (length) of this input
#' key.
#'
#' @examples
#' key <- new(Class = 'rawKey', 
#'            c('Turnover@@001@@@@', 
#'              'Employees@@001@@1@@0', 
#'              'Employees@@001@@0@@', 
#'              'Employees@@001@@1@@1'))
#' KeyToDT(key)
#' 
#' data(ExamplerawKey)
#' KeyToDT(ExamplerawKey)
#' 
#' @export
setGeneric("KeyToDT", function(key){standardGeneric("KeyToDT")})
#'
#' @rdname KeyToDT
#' 
#' @include rawKey-class.R
#' 
#' @export
setMethod(
    f = "KeyToDT",
    signature = c("rawKey"),
    function(key){
        
        StrSplit <- function(x, n, sep){
            
            n <- unique(n)
            if (sep == '@@') {
                
                
                if (n == 1){
                    
                    KeyRegExp <- '[A-Za-z0-9]+'
                                        
                } else {
                    
                    KeyRegExp <- paste0(c('([A-Za-z0-9]+)', 
                                          paste0(rep.int('([A-Za-z0-9]*)', n - 1), collapse = '@@')),
                                        collapse = '@@')
                }
                
            } else {
                
                KeyRegExp <- '(^[A-Za-z0-9]+):([A-Za-z0-9]+$)'
            }
            
            
            for (i in 1:n) {
                
                output[[i]] <- gsub(KeyRegExp, paste0("\\", i), x)
                
            }
            names(output) <- as.character(1:n)
            
            return(output)
        }
        
        
        keyDT <- data.table(OrigKey = key)
        keyDT[, NCol := stringr::str_count(OrigKey, '@@') + 1]
        
        NCol <- unique(keyDT[['NCol']])
        ParsedRawKeyList <- StrSplit(keyDT[['OrigKey']]@.Data, NCol, '@@')
        ParsedRawKeyList <-lapply(ParsedRawKeyList, as.data.table)
        
        output <- Reduce(cbind, ParsedRawKeyList)

        return(output)
    }
)
