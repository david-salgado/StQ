#' @title Return data.table out of a set of parsed keys.
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
            
            n <- unique(n) + 1
            output <- vector('list', n)
            if (sep == '_') {
                
                KeyRegExp <- paste0(rep('([A-Za-z0-9]+:[A-Za-z0-9]+)', n), collapse = '_')
                
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
        keyDT[, NCol := stringr::str_count(OrigKey, '_')]
        keyDTList <- split(keyDT, keyDT[['NCol']])
        ColNames <- names(keyDTList)
       
        ParsedRawKeyList <- lapply(as.list(ColNames), function(NCol){
            
            NCol <- as.integer(NCol)
            KeyValuePair_ <- StrSplit(keyDTList[[as.character(NCol)]][['OrigKey']], NCol, '_')
            KeyValuePairList <- lapply(KeyValuePair_, StrSplit, n = 1, ':')
            return(KeyValuePairList)
            
        })

       
        QualsDT <- lapply(ParsedRawKeyList, function(ListRawKey){
            
            DT <- lapply(ListRawKey, as.data.table)
            DT <- lapply(DT, function(table){
                
                for (col in 1:dim(table)[2]){
                    
                    table[[col]] <- table[[col]]@.Data
                }
                
                return(table)
            })
            DT <- Reduce(cbind, DT)
            
            DTQual <- DT
            ncol <- seq(3, dim(DTQual)[2], 2)
            for (col in ncol){
                
                quals <- unique(DTQual[[col]])
                if(length(quals) > 1){
                    
                    DT <- lapply(quals, function(qual){
                        
                                out <- DTQual[DTQual[[col]] == qual]
                                return(out)
                          })
                }
            }
            
            
            if (class(DT)[1] == 'list')
            {
                
                DT <- lapply(DT, function(List){
                    colNames <- vector('character', dim(List)[2])
                    ncol <- seq(1, length(colNames), 2)
                    for (col in ncol){
                        
                        colNames[col] <- 'Qual'
                        colNames[col + 1] <- unique(List[[col]])
                    }
                    setnames(List, colNames)
                    for (col in 1:length(ncol)){
                        
                        List[, Qual := NULL]
                    }
                    
                    return(List)
                })
                
                #qualNames <- unlist(lapply(DT, function(x){names(x)[dim(x)[2]]}))
                #for(i in 1:length(DT)){
                    
                #    namesDT <- copy(names(DT[[i]]))
                #    nameAdd <- setdiff(qualNames, namesDT)
                #    DT[[i]][, v:= NA]
                #    setnames(DT[[i]], c(namesDT, nameAdd))
                #}
                
                DT <- rbindlist(DT, fill = TRUE)
            }else{
                
                colNames <- vector('character', dim(DT)[2])
                ncol <- seq(1, length(colNames), 2)
                for (col in ncol){
                    
                    colNames[col] <- 'Qual'
                    colNames[col + 1] <- unique(DT[[col]])
                }
                setnames(DT, colNames)
                for (col in 1:length(ncol)){
                    
                    DT[, Qual := NULL]
                }
            }


            return(DT)
        })
        

        output <- rbindlist(QualsDT, fill = TRUE)
        ColNames <- names(output)
        for (col in ColNames){
            
            output[is.na(get(col)), col := '', with = F]
        }
        setcolorder(output, c(setdiff(ColNames, 'IDDD'), 'IDDD'))
        return(output)
    }
)
