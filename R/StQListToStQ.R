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
#' @export
setGeneric("StQListToStQ", function(object){standardGeneric("StQListToStQ")})

#' @rdname StQListToStQ
#'
#'
#' @include StQList-class.R StQ-class.R getData.R getDD.R DatadtToDT.R BuildVNC.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "StQListToStQ",
    signature = c("StQList"),
    function(object){


        DD <- Reduce('+', getDD(object))

        VNCPer <- BuildVNC(list(MicroData = new(Class = 'VNCdt',
                                                .Data = data.table(IDQual = c('Period'),
                                                                   NonIDQual = '',
                                                                   IDDD = '',
                                                                   Period = '.',
                                                                   UnitName = '',
                                                                   InFiles = ''))))
        # Microdt <- new(Class = 'DDdt',data.table(Variable = c('Period'),
        #                                          Sort = c('IDQual'),
        #                                          Class = c('character'),
        #                                          Length = c('8'),
        #                                          Qual1 = '',
        #                                          ValueRegExp = '.+'))

        NewDDdt <- new(Class = 'DDdt',data.table(Variable = c('Period'),
                                                 Sort = c('IDQual'),
                                                 Class = c('character'),
                                                 Length = c('8'),
                                                 Qual1 = '',
                                                 ValueRegExp = '.+'))
                                                 
        
        #DDPer <- new(Class = 'DD', VarNameCorresp = VNCPer, ID = new(Class = 'DDdt'), MicroData = Microdt, ParaData = new(Class = 'DDdt'))
        DDPer <- new(Class = 'DD', VarNameCorresp = VNCPer, ID = NewDDdt, MicroData = NewDDdt, ParaData = NewDDdt)
        DD <- DD + DDPer

        
        DDdtNames.list <- setdiff(slotNames(DD), 'VarNameCorresp')
        DDdt.list <- lapply(DDdtNames.list, function(Name){
                             
            newDDdt <- slot(DD, Name)
            newDDdt.DT <- DatadtToDT(newDDdt)
            ColnewDDdt.DT <- names(newDDdt.DT)
            
            nQual <- length(grep('Qual', ColnewDDdt.DT)) + 1
            newDDdt.DT[Sort == 'IDDD', (paste0('Qual', nQual)) := 'Period']
            setcolorder(newDDdt.DT, c('Variable', 'Sort', 'Class', 'Length', paste0('Qual', 1:nQual), 'ValueRegExp'))
        })
        names(DDdt.list) <- DDdtNames.list
        setID(DD) <- new(Class = 'DDdt', DDdt.list[['ID']])
        setMicroData(DD) <- new(Class = 'DDdt', DDdt.list[['MicroData']])
        setParaData(DD) <- new(Class = 'DDdt', DDdt.list[['ParaData']])
        setAggregates(DD) <- new(Class = 'DDdt', DDdt.list[['Aggregates']])
        setAggWeights(DD) <- new(Class = 'DDdt', DDdt.list[['AggWeights']])
        setOtherDD(DD) <- new(Class = 'DDdt', DDdt.list[['Other']])
        
        
        # newMicroData <- getData(DD)
        # newMD.DT <- DatadtToDT(newMicroData)
        # ColnewMD.DT <- names(newMD.DT)
        # 
        # nQual <- length(grep('Qual', ColnewMD.DT)) + 1
        # newMD.DT[Sort == 'IDDD', (paste0('Qual', nQual)) := 'Period']
        # setcolorder(newMD.DT, c('Variable', 'Sort', 'Class', 'Length', paste0('Qual', 1:nQual), 'ValueRegExp'))
        # setMicroData(DD) <- new(Class = 'DDdt', newMD.DT)


        IDQual <- getIDQual(DD)
        NonIDQual <- getNonIDQual(DD)
        DatadtList <- lapply(getData(object), getData)
        DataList <- lapply(DatadtList, DatadtToDT)
        Periods <- names(DataList)

        for (Per in Periods) {
            set(DataList[[Per]], NULL, 'Period', Per)
            setcolorder(DataList[[Per]], c(intersect(IDQual, names(DataList[[Per]])),
                                           intersect(NonIDQual, names(DataList[[Per]])),
                                           c('IDDD', 'Value')))
        }


        Datadt <- new(Class = 'Datadt', .Data = rbindlist(DataList))
        out <- new(Class = 'StQ', Data = Datadt, DD = DD)

        return(out)
    }
)
