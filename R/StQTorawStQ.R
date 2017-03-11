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
#' @include StQ.R rawStQ.R getDD.R getData.R
#' 
#'  
#' @export
setGeneric("StQTorawStQ", function(Q){standardGeneric("StQTorawStQ")})

#' @rdname StQTorawStQ
#' 
#' @importFrom stringr str_pad
#' 
#' @export
setMethod(
    f = "StQTorawStQ",
    signature = c("StQ"),
    function(Q){
        
        if (dim(getData(Q))[1] == 0){
          
          rawQ <- rawStQ(rawData = data.table(IDDDKey = character(0),
                                              QualKey = character(0),
                                              Value = character(0)),
                         DD = getDD(Q))
          return(rawQ)
        }
      
        DD <- getDD(Q)
        DDdt.list <- setdiff(names(DD), 'VNC')
        DDdt.list <- lapply(DDdt.list, function(Name){DD[[Name]]})
        names(DDdt.list) <- setdiff(names(DD), 'VNC')
        DDdt <- rbindlist(DDdt.list, fill = TRUE)
        setkeyv(DDdt, names(DDdt))
        DDdt <- DDdt[!duplicated(DDdt, by = key(DDdt))]
        IDDDNames <- DDdt[Sort == 'IDDD'][['Variable']]

        QData <- copy(getData(Q))
        setnames(QData, 'IDDD', 'IDDDKey')
        ColNames <- setdiff(names(QData), c('IDDDKey', 'Value'))

        for (col in ColNames){

            Width <- DDdt[Variable == col][['Length']]
            QData[, (col) := stringr::str_pad(get(col), Width, 'right', ' ')]
            
        }

        QData.list <- split(QData, QData[['IDDDKey']])
        QData.list <- QData.list[intersect(names(QData.list), IDDDNames)]

        QData.list <- lapply(names(QData.list), function(VarName){

            QualsDT <- DDdt[Variable == VarName, names(DDdt)[grep('Qual', names(DDdt))], with = F]
            Quals <- t(QualsDT)[, 1]
            Quals <- Quals[Quals != '' & !is.na(Quals)]
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
        rawQ <- rawStQ(rawData = QData, DD = DD)
        return(rawQ)
    }
)
#' @rdname StQTorawStQ
#' 
#' @importFrom stringr str_pad
#' 
#' @export
setMethod(
    f = "StQTorawStQ",
    signature = c("StQList"),
    function(Q){
        
        
        QList <- getData(Q)
        Periods <- RepoTime::getRepo(Q)
        rawQData <- lapply(QList, StQTorawStQ)
        rawQList <- rawStQList(Data = rawQData, Periods = Periods)
        return(rawQList)
    }
)
