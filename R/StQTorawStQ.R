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
<<<<<<< HEAD
#' @include StQ.R rawStQ.R getDD.R getData.R
#' 
#'  
#' @export
setGeneric("StQTorawStQ", function(Q){standardGeneric("StQTorawStQ")})

#' @rdname StQTorawStQ
#' 
=======
#' @export
setGeneric("StQTorawStQ", function(Q){standardGeneric("StQTorawStQ")})
#' @rdname StQTorawStQ
#' 
#' @include StQ-class.R rawKey-class.R rawDatadt-class.R rawStQ-class.R getDD.R getData.R DatadtToDT.R
#' 
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @importFrom stringr str_pad
#' 
#' @export
setMethod(
    f = "StQTorawStQ",
    signature = c("StQ"),
    function(Q){
        
        if (dim(getData(Q))[1] == 0){
          
<<<<<<< HEAD
          rawQ <- rawStQ(rawData = data.table(IDDDKey = character(0),
                                              QualKey = character(0),
                                              Value = character(0)),
                         DD = getDD(Q))
=======
          rawQ <- new(Class = 'rawStQ')
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
          return(rawQ)
        }
      
        DD <- getDD(Q)
<<<<<<< HEAD
        DDdt.list <- setdiff(names(DD), 'VNC')
        DDdt.list <- lapply(DDdt.list, function(Name){DD[[Name]]})
        names(DDdt.list) <- setdiff(names(DD), 'VNC')
        DDdt <- rbindlist(DDdt.list, fill = TRUE)
        setkeyv(DDdt, names(DDdt))
        DDdt <- DDdt[!duplicated(DDdt, by = key(DDdt))]
        IDDDNames <- DDdt[Sort == 'IDDD'][['Variable']]

        QData <- getData(Q)
        setnames(QData, 'IDDD', 'IDDDKey')
        ColNames <- setdiff(names(QData), c('IDDDKey', 'Value'))

        for (col in ColNames){

            Width <- DDdt[Variable == col][['Length']]
=======
        DDdt.list <- setdiff(slotNames(DD), 'VarNameCorresp')
        DDdt.list <- lapply(DDdt.list, function(Name){slot(DD, Name)})
        DDdt <- Reduce('+', DDdt.list, init = DDdt.list[[1]])
        IDDDNames <- DatadtToDT(DDdt)[Sort == 'IDDD'][['Variable']]

        QData <- DatadtToDT(getData(Q))
        setnames(QData, 'IDDD', 'IDDDKey')
        ColNames <- setdiff(names(QData), c('IDDDKey', 'Value'))
        for (col in ColNames){
   
            Width <- DatadtToDT(DDdt)[Variable == col][['Length']]
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            QData[, (col) := stringr::str_pad(get(col), Width, 'right', ' ')]
            
        }

        QData.list <- split(QData, QData[['IDDDKey']])
        QData.list <- QData.list[intersect(names(QData.list), IDDDNames)]
<<<<<<< HEAD

        QData.list <- lapply(names(QData.list), function(VarName){

            QualsDT <- DDdt[Variable == VarName, names(DDdt)[grep('Qual', names(DDdt))], with = F]
            Quals <- t(QualsDT)[, 1]
            Quals <- Quals[Quals != '' & !is.na(Quals)]
            out <- QData.list[[VarName]][, c('IDDDKey', Quals, 'Value'), with = F]
            
=======
        
        QData.list <- lapply(names(QData.list), function(VarName){
     
            QualsDT <- DatadtToDT(DDdt)[Variable == VarName, names(DDdt)[grep('Qual', names(DDdt))], with = F]
            Quals <- t(QualsDT)[, 1]
            Quals <- Quals[Quals != '']
            out <- QData.list[[VarName]][, c('IDDDKey', Quals, 'Value'), with = F]
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
<<<<<<< HEAD
        rawQ <- rawStQ(rawData = QData, DD = DD)
=======
        rawDatadt <- new(Class = 'rawDatadt', .Data = QData)
        rawQ <- new(Class = 'rawStQ', Data = rawDatadt, DD = DD)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(rawQ)
    }
)
#' @rdname StQTorawStQ
#' 
<<<<<<< HEAD
=======
#' @include StQList-class.R rawKey-class.R rawDatadt-class.R rawStQ-class.R rawStQList-class.R getDD.R getData.R
#' 
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
<<<<<<< HEAD
        rawQList <- rawStQList(Data = rawQData, Periods = Periods)
=======
        rawQList <- new(Class = 'rawStQList', Data = rawQData, Periods = Periods)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(rawQList)
    }
)
