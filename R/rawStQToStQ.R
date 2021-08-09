#' @title Generates an object of class \link{StQ} from the input \linkS4class{rawStQ} object.
#'
#' @description \code{rawStQToStQ} returns an object of class \link{StQ} from the input
#' \linkS4class{rawStQ} object.
#'
#' @param rawQ Object of class \linkS4class{rawStQ} whose slot Data has the  key-value pair
#' structure.
#'
#' @return Object of class \link{StQ} whose slot Data has at least the columns \code{IDDD}
#' and \code{Value}.
#'
#' @examples
#' data(ExamplerawStQ)
#' StQ <- rawStQToStQ(ExamplerawStQ)
#' str(StQ)
#' 
#' @include rawStQ.R StQ.R getDD.R getData.R
#'
#' @export
setGeneric("rawStQToStQ", function(rawQ){standardGeneric("rawStQToStQ")})
#' @rdname rawStQToStQ
#'
#' @importFrom stringi stri_sub stri_trim_right
#'
#' @export
setMethod(
    f = "rawStQToStQ",
    signature = c("rawStQ"),
    function(rawQ){

        DD <- getDD(rawQ)

        DDdt.list <- setdiff(names(DD), 'VNC')
        DDdt.list <- lapply(DDdt.list, function(Name){DD[[Name]]})
        DDdt <- rbindlist(DDdt.list, fill = TRUE)

        rawDT <- getData(rawQ)
        setnames(rawDT, 'IDDDKey', 'IDDD')
        rawData.list <- split(rawDT, rawDT[['IDDD']])

        Quals.list <- lapply(names(rawData.list), function(VarName){

                QualsDT <- DDdt[Variable == VarName, names(DDdt)[grep('Qual', names(DDdt))], with = F]
                Quals <- t(QualsDT)[, 1]
                Quals <- Quals[Quals != '' & !is.na(Quals)]

                Lengths <- lapply(unique(Quals), function(Qual){

                    LocalOut <- DDdt[Variable == Qual]
                    LocalOut <- unique(LocalOut[['Length']])
                    return(LocalOut)
                })

                Lengths <- unlist(Lengths)
                names(Quals) <- Lengths
                return(Quals)
        })

        names(Quals.list) <- names(rawData.list)

        Data.list <- lapply(names(rawData.list), function(VarName){

            QualKey <- data.table(Quals = rawData.list[[VarName]][['QualKey']])
            fin <- cumsum(names(Quals.list[[VarName]]))
            init <- fin + 1
            init <- c(1, init[-length(init)])
            out <- vector(mode = 'list', length = length(init))
            out <- lapply(1:length(init), function(i){

                outLocal <- stri_sub(QualKey[['Quals']], from = init[i], to = fin[i])
                outLocal <- stri_trim_right(outLocal)

            })

            out <- as.data.table(out)
            setnames(out, Quals.list[[VarName]])
            out[, IDDD := rawData.list[[VarName]][['IDDD']]]
            out[, Value := rawData.list[[VarName]][['Value']]]
            return(out)
        })

        Data <- rbindlist(Data.list, fill = TRUE)
        Names <- intersect(c(getIDQual(getDD(rawQ)), getNonIDQual(getDD(rawQ)), 'IDDD', 'Value'), names(Data))
        setcolorder(Data, Names)
        ColNames <- names(Data)
        for (col in ColNames){

            Data[is.na(get(col)), (col) := '']
        }

        if(nrow(Data) == 0){
            Data[, IDDD := gsub("(\\w+)", "\\1", Data$V1, perl = TRUE)][
                ,Value := gsub("(\\w+)", "\\1", Data$V1, perl = TRUE)]
        }

        Q <- StQ(Data, DD)
        
        return(Q)
    }
)
#' @rdname rawStQToStQ
#'
#' @importFrom stringi stri_sub
#'
#' @export
setMethod(
    f = "rawStQToStQ",
    signature = c("rawStQList"),
    function(rawQ){


        rawQList <- getData(rawQ)
        Periods <- RepoTime::getRepo(rawQ)
        QData <- lapply(rawQList, rawStQToStQ)
        QList <- new(Class = 'StQList', Data = QData, Periods = Periods)
        return(QList)
    }
)
