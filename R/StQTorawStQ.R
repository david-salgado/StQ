#' @title Generates an object of class \linkS4class{rawStQ} from the input
#' \linkS4class{StQ} object.
#'
#' @description \code{StQTorawStQ} returns an object of class
#' \linkS4class{rawStQ} from the input\linkS4class{StQ} object. 
#' 
#' @param Q Object of class \linkS4class{StQ} whose slot Data has at least the 
#' columns \code{IDDD} and \code{Value}.
#'
#' @return Object of class \linkS4class{rawStQ} whose slot Data has the
#' key-value pair structure.
#'
#' @examples
#' library(data.table)
#' data(ExampleStQ)
#' newrawQ <- StQTorawStQ(ExampleStQ)
#'  
#' @export
setGeneric("StQTorawStQ", function(Q){standardGeneric("StQTorawStQ")})
#' @rdname StQTorawStQ
#' 
#' @include StQ-class.R rawKey-class.R rawDatadt-class.R rawStQ-class.R getDD.R getData.R DTToKey.R
#' 
#' @importFrom stringr str_pad
#' 
#' @export
setMethod(
    f = "StQTorawStQ",
    signature = c("StQ"),
    function(Q){
        
        DD <- getDD(Q)
        DDdt.list <- setdiff(slotNames(DD), 'VarNameCorresp')
        DDdt.list <- lapply(DDdt.list, function(Name){slot(DD, Name)})
        DDdt <- Reduce('+', DDdt.list, init = DDdt.list[[1]])
        IDDDNames <- DDdt[Sort == 'IDDD'][['Variable']]
        
        QData <- DatadtToDT(getData(Q))
        setnames(QData, 'IDDD', 'IDDDKey')
        ColNames <- setdiff(names(QData), c('IDDDKey', 'Value'))
        for (col in ColNames){
            
            Width <- DDdt[Variable == col][['Length']]
            QData[, col := stringr::str_pad(get(col), Width, 'right', ' '), with = F]
            
        }
        
        QData.list <- split(QData, QData[['IDDDKey']])[IDDDNames]
        QData.list <- lapply(names(QData.list), function(VarName){
            
            QualsDT <- DDdt[Variable == VarName, names(DDdt)[grep('Qual', names(DDdt))], with = F]
            Quals <- t(QualsDT)[, 1]
            Quals <- Quals[Quals != '']
            out <- QData.list[[VarName]][, c('IDDDKey', Quals, 'Value'), with = F]
            if (length(Quals) == 1){
                
                out[, QualKey := get(Quals)]
                
            } else {
                
                out[, QualKey := '']

                for (qual in Quals){
                    
                    out[, QualKey := paste0(QualKey, get(qual))]
                    
                }
            }
            out <- out[, c('IDDDKey', 'QualKey', 'Value'), with = F]
            return(out)        
            
        })

        QData <- rbindlist(QData.list)

        Value <- QData[['Value']]
        QData[, Value := NULL]
        key <- new(Class = 'rawKey', QData)
        rawDatadt <- BuildrawDatadt(key, Value)
        rawQ <- new(Class = 'rawStQ', Data = rawDatadt, DD = DD)
        return(rawQ)
    }
)
#' @rdname StQTorawStQ
#' 
#' @include StQList-class.R rawKey-class.R rawDatadt-class.R rawStQ-class.R getDD.R getData.R DTToKey.R
#' 
#' @export
setMethod(
    f = "StQTorawStQ",
    signature = c("StQList"),
    function(Q){
        
        
        QList <- getData(Q)
        Periods <- getRepo(Q)
        rawQData <- lapply(QList, StQTorawStQ)
        rawQList <- new(Class = 'rawStQList', Data = rawQData, Periods = Periods)
        return(rawQList)
    }
)
