#' @title Return a character vector of keys out of a \linkS4class{data.table}.
#'
#' @description \code{DTToKey} returns a character vector of keys from the input
#' \linkS4class{data.table} whose columns are the different qualifiers with as 
#' many rows as components (length) of this input key. 
#' 
#' @param DT \linkS4class{data.table} with the key components.
#'
#' @return Object of class \linkS4class{rawKey} with the keys of the input
#'
#' @examples
#' key <- new(Class = 'rawKey', 
#'            c('Turnover@@001@@@@', 
#'              'Employees@@001@@1@@0', 
#'              'Employees@@001@@0@@', 
#'              'Employees@@001@@1@@1'))
#' keyDT <- DTToKey(KeyToDT(key))
#' 
#' @export
setGeneric("DTToKey", function(DT){standardGeneric("DTToKey")})
#' @rdname DTToKey
#' 
#' @include rawKey-class.R
#' 
#' @export
setMethod(
    f = "DTToKey",
    signature = c("data.table"),
    function(DT){
        
        ColNames <- names(DT)
        if (!'IDDD' %in% ColNames) stop('[StQ::DTToKey] IDDD must a column of the data.table.')
        ColNames <- c('IDDD', setdiff(ColNames, 'IDDD'))
        key <- vector('character', dim(DT)[1])
        for(index.col in seq(along = ColNames)){
            
            if (index.col == 1) { 
                
                #key <- paste0(paste0(ColNames[index.col], ':'), DT[[ColNames[index.col]]])
                key <- DT[[ColNames[index.col]]]
            
            } else {
                
                    key <- ifelse(!is.na(DT[[ColNames[index.col]]]),
                                  ifelse(DT[[ColNames[index.col]]] == '', 
                                         paste0(key, '@@'),
                                         paste0(key, '@@', DT[[ColNames[index.col]]])),
                                  paste0(key, '@@', ' '))
            }
        }

        OutKey <- new(Class = 'rawKey', key)
        return(OutKey)
    }
)
