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
<<<<<<< HEAD
#' 
#' @include rawStQ.R StQ.R getDD.R getData.R
=======
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @export
setGeneric("rawStQToStQ", function(rawQ){standardGeneric("rawStQToStQ")})
#' @rdname rawStQToStQ
#'
<<<<<<< HEAD
=======
#' @include rawStQ-class.R Datadt-class.R StQ-class.R getDD.R getData.R DatadtToDT.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @importFrom stringi stri_sub stri_trim_right
#'
#' @export
setMethod(
    f = "rawStQToStQ",
    signature = c("rawStQ"),
    function(rawQ){

        DD <- getDD(rawQ)
<<<<<<< HEAD
        DDdt.list <- setdiff(names(DD), 'VNC')
        DDdt.list <- lapply(DDdt.list, function(Name){DD[[Name]]})
        DDdt <- rbindlist(DDdt.list, fill = TRUE)

        rawDT <- getData(rawQ)
=======
        DDdt.list <- setdiff(slotNames(DD), 'VarNameCorresp')
        DDdt.list <- lapply(DDdt.list, function(Name){slot(DD, Name)})
        DDdt <- Reduce('+', DDdt.list, init = DDdt.list[[1]])
        DDdt <- DatadtToDT(DDdt)

        rawDT <- DatadtToDT(getData(rawQ))
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        setnames(rawDT, 'IDDDKey', 'IDDD')
        rawData.list <- split(rawDT, rawDT[['IDDD']])

        Quals.list <- lapply(names(rawData.list), function(VarName){

                QualsDT <- DDdt[Variable == VarName, names(DDdt)[grep('Qual', names(DDdt))], with = F]
                Quals <- t(QualsDT)[, 1]
<<<<<<< HEAD
                Quals <- Quals[Quals != '' & !is.na(Quals)]

                Lengths <- lapply(unique(Quals), function(Qual){

                    LocalOut <- DDdt[Variable == Qual]
                    LocalOut <- unique(LocalOut[['Length']])
                    return(LocalOut)
                })

                Lengths <- unlist(Lengths)
=======
                Quals <- Quals[Quals != '']
                Lengths <- unlist(lapply(Quals, function(Qual){DDdt[Variable == Qual][['Length']]}))
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
<<<<<<< HEAD
        #Datadt <- new(Class = 'Datadt', Data)
        #Q <- new(Class = 'StQ', Data = Datadt, DD = DD)
        Q <- StQ(Data, DD)
        
=======
        Datadt <- new(Class = 'Datadt', Data)
        Q <- new(Class = 'StQ', Data = Datadt, DD = DD)

>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(Q)
    }
)
#' @rdname rawStQToStQ
#'
<<<<<<< HEAD
=======
#' @include rawStQList-class.R Datadt-class.R StQ-class.R StQList-class.R getDD.R getData.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @importFrom stringi stri_sub
#'
#' @import RepoTime
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
