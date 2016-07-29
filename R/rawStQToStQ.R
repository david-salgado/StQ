#' @title Generates an object of class \linkS4class{StQ} from the input \linkS4class{rawStQ} object.
#'
#' @description \code{rawStQToStQ} returns an object of class \linkS4class{StQ} from the input
#' \linkS4class{rawStQ} object. 
#' 
#' @param rawQ Object of class \linkS4class{rawStQ} whose slot Data has the  key-value pair
#' structure.
#'
#' @return Object of class \linkS4class{StQ} whose slot Data has at least the columns \code{IDDD}
#' and \code{Value}.
#'
#' @examples
#' data(ExamplerawStQ)
#' StQ <- rawStQToStQ(ExamplerawStQ)
#' str(StQ)
#' 
#' @export
setGeneric("rawStQToStQ", function(rawQ){standardGeneric("rawStQToStQ")})
#' @rdname rawStQToStQ
#' 
#' @include rawStQ-class.R Datadt-class.R StQ-class.R getDD.R getData.R
#' 
#' @importFrom stringi stri_sub stri_trim_right
#' 
#' @export
setMethod(
    f = "rawStQToStQ",
    signature = c("rawStQ"),
    function(rawQ){
        
        DD <- getDD(rawQ)
        DDdt.list <- setdiff(slotNames(DD), 'VarNameCorresp')
        DDdt.list <- lapply(DDdt.list, function(Name){slot(DD, Name)})
        DDdt <- Reduce('+', DDdt.list, init = DDdt.list[[1]])
        
        rawDT <- DatadtToDT(getData(rawQ))
        setnames(rawDT, 'IDDDKey', 'IDDD')
        rawData.list <- split(rawDT, rawDT[['IDDD']])
        
        Quals.list <- lapply(names(rawData.list), function(VarName){
            
                QualsDT <- DDdt[Variable == VarName, names(DDdt)[grep('Qual', names(DDdt))], with = F]
                Quals <- t(QualsDT)[, 1]
                Quals <- Quals[Quals != '']
                Lengths <- unlist(lapply(Quals, function(Qual){DDdt[Variable == Qual][['Length']]}))
                names(Quals) <- Lengths
                return(Quals)
            })
        names(Quals.list) <- names(rawData.list)

        Data.list <- lapply(names(rawData.list), function(VarName){
            
            QualKey <- data.table(Quals = rawData.list[[VarName]][['QualKey']])

            fin <- cumsum(names(Quals.list[[VarName]]))
            init <- fin + 1
            init <- c(1, init[-length(init)])
            QualKey[, Quals.list[[VarName]] :={out <- stri_sub(Quals, from = init, to = fin); as.list(out)}, by = Quals]
            QualKey[, Quals := NULL]
            ColNames <- names(QualKey)
            for (col in ColNames){
                
                QualKey[, col := stri_trim_right(get(col)), with = F]
                
            }
            QualKey[, IDDD := rawData.list[[VarName]][['IDDD']]]
            QualKey[, Value := rawData.list[[VarName]][['Value']]]
            return(QualKey)
        })

        Data <- rbindlist(Data.list, fill = TRUE)
        colData <- names(Data)
        for (col in colData){
            
            Data[is.na(get(col)), col := '', with = F]
        }
        setcolorder(Data, c(setdiff(names(Data), c('IDDD', 'Value')), 'IDDD', 'Value'))
        Datadt <- new(Class = 'Datadt', Data)
        Q <- new(Class = 'StQ', Data = Datadt, DD = DD)
        
        return(Q)
    }
)
#' @rdname rawStQToStQ
#' 
#' @include rawStQList-class.R Datadt-class.R StQ-class.R StQList-class.R getDD.R getData.R
#' 
#' @importFrom stringi stri_sub
#' 
#' @export
setMethod(
    f = "rawStQToStQ",
    signature = c("rawStQList"),
    function(rawQ){
        
        
        rawQList <- getData(rawQ)
        Periods <- getRepo(rawQ)
        QData <- lapply(rawQList, rawStQToStQ)
        QList <- new(Class = 'StQList', Data = QData, Periods = Periods)
        return(QList)
    }
)
