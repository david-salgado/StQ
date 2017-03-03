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
#' data(ExampleStQ)
#' StQTorawStQ(ExampleStQ)
#'  
#' @export
setGeneric("StQTorawStQ", function(Q){standardGeneric("StQTorawStQ")})
#' @rdname StQTorawStQ
#' 
#' @include StQ-class.R rawKey-class.R rawDatadt-class.R rawStQ-class.R getDD.R getData.R DatadtToDT.R
#' 
#' @importFrom stringr str_pad
#' 
#' @export
setMethod(
    f = "StQTorawStQ",
    signature = c("StQ"),
    function(Q){
        
        if (dim(getData(Q))[1] == 0){
          
          rawQ <- new(Class = 'rawStQ')
          return(rawQ)
        }
      
        DD <- getDD(Q)
        DDdt.list <- setdiff(slotNames(DD), 'VarNameCorresp')
        DDdt.list <- lapply(DDdt.list, function(Name){slot(DD, Name)})
        DDdt <- Reduce('+', DDdt.list, init = DDdt.list[[1]])
        IDDDNames <- DatadtToDT(DDdt)[Sort == 'IDDD'][['Variable']]

        QData <- DatadtToDT(getData(Q))
        setnames(QData, 'IDDD', 'IDDDKey')
        ColNames <- setdiff(names(QData), c('IDDDKey', 'Value'))
        for (col in ColNames){
   
            Width <- DatadtToDT(DDdt)[Variable == col][['Length']]
            QData[, (col) := stringr::str_pad(get(col), Width, 'right', ' ')]
            
        }

        QData.list <- split(QData, QData[['IDDDKey']])
        QData.list <- QData.list[intersect(names(QData.list), IDDDNames)]
        
        QData.list <- lapply(names(QData.list), function(VarName){
     
            QualsDT <- DatadtToDT(DDdt)[Variable == VarName, names(DDdt)[grep('Qual', names(DDdt))], with = F]
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
        setkeyv(QData, names(QData))
        QData <- QData[!duplicated(QData, by = key(QData))]
        rawDatadt <- new(Class = 'rawDatadt', .Data = QData)
        rawQ <- new(Class = 'rawStQ', Data = rawDatadt, DD = DD)
        return(rawQ)
    }
)
#' @rdname StQTorawStQ
#' 
#' @include StQList-class.R rawKey-class.R rawDatadt-class.R rawStQ-class.R rawStQList-class.R getDD.R getData.R
#' 
#' @importFrom stringr str_pad
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
