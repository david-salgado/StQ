#' @title Return data.table out of a set of parsed keys.
#' 
#' @description \code{KeyToDT} returns a \linkS4class{data.table} whose columns are the different
#' qualifiers parsed in the input key with as many raws as components (length) of this input key.
#' 
#' @param key key implemented either through the class \linkS4class{rawKey} or 
#' other.
#'
#' @return \linkS4class{data.table} with one column per each different qualifier
#' found in the input key and as many rows as components (length) of this input
#' key.
#'
#' @examples
#' key <- new(Class = 'rawKey', c('IASS@@9644947400S@@@@@@@@@@@@@@@@@@@@@@@@IASSCifraNeg',
#'                                'IASS@@9644947400S@@@@1@@1@@@@@@@@@@@@@@@@@@IASSEmpleo',
#'                                'IASS@@9644947400S@@03@@@@@@@@@@@@@@@@@@@@@@IASSLPCifraNeg'))
#'                         
#' key <- new(Class = 'rawKey', c('IASS@@9644947400S@@@@@@@@@@@@IASSCifraNeg',
#'                                'IASS@@9644947400S@@@@1@@1@@@@@@IASSEmpleo',
#'                                'IASS@@9644947400S@@03@@@@@@@@@@IASSLPCifraNeg'))
#'                         
#' names <- c('NOrden', 'CCAA', 'EsRemuner', 'TipoRem', 'RamaCNAE09', 'IDRefPond1', 'VarPonder',
#'             'DivisionCNAE09', 'IDRefPond2', 'SectorCNAE09', 'GeneralOtrosCNAE09', 'IDRefPond3',
#'             'IDDD')                        
#' KeyToDT(key, names)
#' 
#' data(ExamplerawKey)
#' KeyToDT(ExamplerawKey)
#' 
#' @export
setGeneric("KeyToDT", function(key, names){standardGeneric("KeyToDT")})
#'
#' @rdname KeyToDT
#' 
#' @include rawKey-class.R
#' 
#' @export
setMethod(
    f = "KeyToDT",
    signature = c("rawKey"),
    function(key, names){
        # 
        # StrSplit <- function(x, n, sep){
        # 
        #     if (sep == '@@') {
        # 
        #         KeyRegExp <- paste0(c('([A-Za-z]+)',
        #                              paste0(rep('([A-Za-z0-9]*|[:blank])', n - 1), collapse = '@@')) ,
        #                              collapse = '@@')
        # 
        #     } else {
        # 
        #         KeyRegExp <- '(^[A-Za-z0-9]+):([A-Za-z0-9]+$)'
        #     }
        # 
        #     output <- lapply(seq(along = x), function(i){
        # 
        #         out <- unlist(lapply(1:n, function(j){
        # 
        #             gsub(KeyRegExp, paste0("\\", j), x[i])
        #         }))
        # 
        #         return(out)
        #     })
        # 
        #     #names(output) <- as.character(1:(n - 1))
        #     return(output)
        # }
        
        
        NCol <- unique(stringr::str_count(key, '@@')) + 1
        KeyRegExp <- paste0(c('([A-Za-z]+)',
                                  paste0(rep('([A-Za-z0-9]*|[:blank])', NCol - 1), collapse = '@@')) ,
                                collapse = '@@')

        
        #ParsedRawKeyList <- StrSplit(keyDT[['OrigKey']], NCol, '@@')

        ParsedRawKeyList <- stringr::str_match(key, KeyRegExp)[, 2:(NCol + 1)]
        output <- as.data.table(ParsedRawKeyList)[, V1 := NULL]
        names(output) <- names
        
        # RepoDD[NOMID != "", NOMID] -> NOMID
        # RepoDD[NOMCALIFICADOR != "", NOMCALIFICADOR] -> NOMCALIFICADOR
        # names <- c(NOMID, NOMCALIFICADOR, 'IDDD')
        # names(QualsDT) <- names
        
        return(output)
    }
)

   
