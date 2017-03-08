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
<<<<<<< HEAD
#' @include StQList.R StQ.R getData.R getDD.R BuildVNC.R plus.DD.R DD.R BuildDD.R setID.R setMicroData.R setParaData.R setAggregates.R setAggWeights.R setOther.R
#' 
#' @import data.table
#' 
=======
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @export
setGeneric("StQListToStQ", function(object){standardGeneric("StQListToStQ")})

#' @rdname StQListToStQ
#'
<<<<<<< HEAD
=======
#'
#' @include StQList-class.R StQ-class.R getData.R getDD.R DatadtToDT.R BuildVNC.R
#'
#' @import data.table
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @export
setMethod(
    f = "StQListToStQ",
    signature = c("StQList"),
    function(object){


        DD <- Reduce('+', getDD(object))

<<<<<<< HEAD
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
=======
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
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            ColnewDDdt.DT <- names(newDDdt.DT)
            
            nQual <- length(grep('Qual', ColnewDDdt.DT)) + 1
            newDDdt.DT[Sort == 'IDDD', (paste0('Qual', nQual)) := 'Period']
            setcolorder(newDDdt.DT, c('Variable', 'Sort', 'Class', 'Length', paste0('Qual', 1:nQual), 'ValueRegExp'))
        })
        names(DDdt.list) <- DDdtNames.list
<<<<<<< HEAD
        setID(DD) <- DDdt.list[['ID']]
        setMicroData(DD) <- DDdt.list[['MicroData']]
        setParaData(DD) <- DDdt.list[['ParaData']]
        setAggregates(DD) <- DDdt.list[['Aggregates']]
        setAggWeights(DD) <- DDdt.list[['AggWeights']]
        setOtherDD(DD) <- DDdt.list[['Other']]

        IDQual <- getIDQual(DD)
        NonIDQual <- getNonIDQual(DD)
        DataList <- getData(object)
=======
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
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        Periods <- names(DataList)

        for (Per in Periods) {
            set(DataList[[Per]], NULL, 'Period', Per)
            setcolorder(DataList[[Per]], c(intersect(IDQual, names(DataList[[Per]])),
                                           intersect(NonIDQual, names(DataList[[Per]])),
                                           c('IDDD', 'Value')))
        }


<<<<<<< HEAD
        Datadt <- rbindlist(DataList)
        out <- StQ(Data = Datadt, DD = DD)
=======
        Datadt <- new(Class = 'Datadt', .Data = rbindlist(DataList))
        out <- new(Class = 'StQ', Data = Datadt, DD = DD)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8

        return(out)
    }
)
