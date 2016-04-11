#' @title Return a character vector of keys out of a \linkS4class{data.table}
#'
#' @description \code{DTToKey} returns a character vector of keys from the input
#' \linkS4class{data.table} whose columns are the different qualifiers with as 
#' many rows as components (length) of this input key. 
#' 
#' @param DT \linkS4class{data.table} with the key components.
#'
#' @return \linkS4class{data.table} with one column per each different qualifier
#' found in the input key and as many rows as components (length) of this input
#' key.
#'
#' @examples
#' key <- new(Class = 'rawKey', c('ID:391092SS_Turnover:9834.3_Province:09',
#'                                'ID:asd2SS',
#'                                'ID:1231_Employees:841_NACE:0502'))
#' DTToKey(KeyToDT(key))
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
        key <- vector('character', dim(DT)[1])
        for(index.col in seq(along = ColNames)){
            
            if (index.col == 1) { 
                
                key <- paste0(paste0(ColNames[index.col], ':'), DT[[ColNames[index.col]]])
            
            } else {
                               
                key <- ifelse(DT[[ColNames[index.col]]] == '', 
                              key,
                              paste0(key, '_', ColNames[index.col], ':', DT[[ColNames[index.col]]]))
            }
        }
        OutKey <- new(Class = 'rawKey', key)
        return(OutKey)
    }
)
