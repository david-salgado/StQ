#' @title Return a \linkS4class{data.table} out of a set of parsed keys
#'
#' @description \code{KeyToDT} returns a \linkS4class{data.table} whose columns
#' are the different qualifiers parsed in the input key with as many raws as
#' components (length) of this input key. 
#' 
#' @param key key implemented either through the class \linkS4class{rawKey} or 
#' other.
#' 
#' @param DD object DD containing the definition and order of the qualifiers.
#'
#' @return \linkS4class{data.table} with one column per each different qualifier
#' found in the input key and as many rows as components (length) of this input
#' key.
#'
#' @examples
#' data(ExamplerawKey)
#' data(ExampleDD)
#' KeyToDT(ExamplerawKey, ExampleDD)
#' 
#' @export
setGeneric("KeyToDT", function(key, DD){standardGeneric("KeyToDT")})
#'
#' @rdname KeyToDT
#' 
#' @include rawKey-class.R
#' 
#' @importFrom stringi stri_replace_all_regex
#' 
#' @export
setMethod(
    f = "KeyToDT",
    signature = c("rawKey", "DD"),
    function(key, DD){
        
        StrSplit <- function(x, n, sep){
            
            n <- unique(n)
            if (sep == '@@') {
                
                
                if (n == 1){
                    
                    KeyRegExp <- '^[A-Za-z0-9]+'
                                        
                } else {
                    
                    KeyRegExp <- paste0(c('^([A-Za-z0-9]+)', 
                                          paste0(rep.int('([A-Za-z0-9]*)', n - 1), collapse = '@@')),
                                        collapse = '@@')
                }
                
                ## [1] "jakl"
            } else {
                
                KeyRegExp <- '(^[A-Za-z0-9]+):([A-Za-z0-9]+$)'
            }
            
            output <- list()
            for (i in 1:n) {
                
                output[[i]] <- stringi::stri_replace_all_regex(x, KeyRegExp, paste0("$", i))

                
            }
            names(output) <- as.character(1:n)
            
            return(output)
        }
        
        
        keyDT <- data.table(OrigKey = key)
        keyDT[, NCol := stringr::str_count(OrigKey, '@@') + 1]

        NCol <- unique(keyDT[['NCol']])
        ParsedRawKeyList <- StrSplit(keyDT[['OrigKey']]@.Data, NCol, '@@')
        ParsedRawKeyList <- lapply(ParsedRawKeyList, as.data.table)
        
        output <- Reduce(cbind, ParsedRawKeyList)
        
        QualNames <- list()
        for (sl in setdiff(slotNames(DD), 'VarNameCorresp')){
            
            if (dim(slot(DD, sl))[1] == 0) next
            QualNames[[sl]] <- slot(DD, sl)[Sort != 'IDDD'][, list(Variable, QualOrder)]
            
        }
        
        QualNames <- rbindlist(QualNames)
        setkeyv(QualNames, names(QualNames))
        QualNames <- QualNames[!duplicated(QualNames)]
        setkeyv(QualNames, 'QualOrder')
        QualNames <- c('IDDD', QualNames[['Variable']])

        if (length(QualNames) != dim(output)[2]) {
            
            stop('[StQ::KeyToDT] The number of qualifiers in the key and in the DD object are not the same.')
        }
        
        setnames(output, QualNames)
        return(output)
    }
)
