#' @title Method to transform an \linkS4class{StQList} object into a list of StQs objects
#'
#' @description \code{StQListToStQ} transform an \linkS4class{StQList} object into a list of StQs 
#' objects with a new variable "Period" to take into account the interval period times related to 
#' \linkS4class{StQList} object.
#'
#' This method creates a variable with the name \code{Period} in Data slots with the period related
#' to each Data and adds this variable to each DD slot of the new StQs objects.
#'
#' @param object Object of class \linkS4class{StQList} to be transformed.
#'
#' @return a list of objects of class \linkS4class{StQ}.
#'
#' @include StQList.R StQ.R getData.R getDD.R BuildVNC.R plus.DD.R DD.R BuildDD.R setID.R setMicroData.R setParaData.R setAggregates.R setAggWeights.R setOther.R
#' 
#' @import data.table
#' 
#' @export
setGeneric("StQListToStQ", function(object){standardGeneric("StQListToStQ")})

#' @rdname StQListToStQ
#'
#' @export
setMethod(
    f = "StQListToStQ",
    signature = c("StQList"),
    function(object){


        DD <- Reduce('+', getDD(object))

        VNCPer <- BuildVNC(list(MicroData = data.table(IDQual = c('Period'),
                                                       NonIDQual = '',
                                                       IDDD = '',
                                                       Period = '.',
                                                       UnitName = '',
                                                       InFiles = '')))
        NewDDdt <- data.table(Variable = c('Period'),
                              Sort = c('IDQual'),
                              Class = c('character'),
                              Length = c('8'),
                              Qual1 = '',
                              ValueRegExp = '.+')
                                                 
        
        DDPer <- DD(VNC = VNCPer, ID = NewDDdt, MicroData = NewDDdt, ParaData = NewDDdt)
        DD <- DD + DDPer

        
        DDdtNames.list <- setdiff(names(DD), 'VNC')
        DDdt.list <- lapply(DDdtNames.list, function(Name){
                             
            newDDdt.DT <- DD[[Name]]
            ColnewDDdt.DT <- names(newDDdt.DT)
            
            nQual <- length(grep('Qual', ColnewDDdt.DT)) + 1
            newDDdt.DT[Sort == 'IDDD', (paste0('Qual', nQual)) := 'Period']
            setcolorder(newDDdt.DT, c('Variable', 'Sort', 'Class', 'Length', paste0('Qual', 1:nQual), 'ValueRegExp'))
        })
        names(DDdt.list) <- DDdtNames.list

        setID(DD) <- DDdt.list[['ID']]
        setMicroData(DD) <- DDdt.list[['MicroData']]
        setParaData(DD) <- DDdt.list[['ParaData']]
        setAggregates(DD) <- DDdt.list[['Aggregates']]
        setAggWeights(DD) <- DDdt.list[['AggWeights']]
        setOtherDD(DD) <- DDdt.list[['Other']]

        IDQual <- getIDQual(DD)
        NonIDQual <- getNonIDQual(DD)
        DataList <- getData(object)
        Periods <- names(DataList)

        for (Per in Periods) {
            set(DataList[[Per]], NULL, 'Period', Per)
            setcolorder(DataList[[Per]], c(intersect(IDQual, names(DataList[[Per]])),
                                           intersect(NonIDQual, names(DataList[[Per]])),
                                           c('IDDD', 'Value')))
        }


        Datadt <- rbindlist(DataList)
        out <- StQ(Data = Datadt, DD = DD)

        return(out)
    }
)
