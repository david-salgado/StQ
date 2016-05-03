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
#' key <- new(Class = 'rawKey', c('IDDD:IASSEmpleo_Norden:391092SS_EsRemuner:1_TipoRemuner:1',
#'                                'IDDD:IASSCifraNeg_Norden:asd2SS',
#'                                'IDDD:IASSLPCifraNeg_Norden:1231_CCAA:01'))
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
