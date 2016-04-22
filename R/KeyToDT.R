#' @title Return \linkS4class{data.table} out of a set of parsed keys.
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
#' key <- new(Class = 'rawKey', c('IDDD:IASSEmpleo_Norden:391092SS_EsRemuner:1_TipoRemuner:1',
#'                                'IDDD:IASSCifraNeg_Norden:asd2SS',
#'                                'IDDD:IASSLPCifraNeg_Norden:1231_CCAA:01'))
#' KeyToDT(key)
#' 
#' @export
setGeneric("KeyToDT", function(key){standardGeneric("KeyToDT")})
#' @rdname KeyToDT
#' 
#' @include rawKey-class.R
#' 
#' @export
setMethod(
    f = "KeyToDT",
    signature = c("rawKey"),
    function(key){
        
        ParsedKey <- lapply(strsplit(key, "_"), function(KeyVal){
            
            ParsedKeyVal <- strsplit(KeyVal, split = ':')
            ParsedKeyVal <- lapply(ParsedKeyVal, 'names<-', c('Key', 'Value'))
            Matrix <- matrix(unlist(lapply(ParsedKeyVal, `[`, 'Value')), 
                             ncol = length(ParsedKeyVal), 
                             nrow = 1, 
                             dimnames = list(NULL, 
                                             unlist(lapply(ParsedKeyVal, 
                                                           `[`, 
                                                           'Key'))))
            DT <- as.data.table(Matrix)
            return(DT)
        })
        
        outDT <- rbindlist(ParsedKey, use.names = TRUE, fill = TRUE)
        for (col in names(outDT)){
            
            outDT[is.na(get(col)), col := '', with = F]
        }
        
        return(outDT)
    }
)
