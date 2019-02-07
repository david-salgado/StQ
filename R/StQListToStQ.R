#' @title Method to transform an \linkS4class{StQList} object into a StQ object
#'
#' @description \code{StQListToStQ} transform an \linkS4class{StQList} object into a StQ object
#' with a new variable "Period" to take into account the interval period times related to
#' \linkS4class{StQList} object.
#'
#' This method creates a variable with the name \code{Period} in Data slot with the period related
#' to each Data and adds this variable to DD slot of the new StQ object.
#'
#' @param object Object of class \linkS4class{StQList} to be transformed.
#'
#' @return object of class \linkS4class{StQ}.
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

        DDold <- Reduce('+', getDD(object))
        auxVNCdt <- data.table(IDQual = c('Period'),
                               NonIDQual = '',
                               IDDD = '',
                               Period = '',
                               UnitName = 'Periodo',
                               InFiles = '')

        VNC <- getVNC(DDold)
        VNCnames <- union(names(VNC), c('ID', 'MicroData', 'ParaData', 'Other', 'AggWeights', 'Aggregates'))
        VNClist <- lapply(VNCnames, function(name){auxVNCdt})
        names(VNClist) <- VNCnames
        VNCPer <- do.call(BuildVNC, list(VNClist))

        NewDDdt <- data.table(Variable = c('Period'),
                              Sort = c('IDQual'),
                              Class = c('character'),
                              Length = c('8'),
                              Qual1 = '',
                              ValueRegExp = '.+')
        DDPer <- DD(VNC = VNCPer, ID = NewDDdt, MicroData = NewDDdt, ParaData = NewDDdt)


        DD <- DDold + DDPer

        DDdtNames.list <- setdiff(names(DD), 'VNC')
        DDdt.list <- lapply(DDdtNames.list, function(Name){

            newDDdt.DT <- copy(DD[[Name]])
            ColnewDDdt.DT <- names(newDDdt.DT)
            nIDQual <- length(getIDQual(DDold, Name))
            nQual <- length(grep('Qual', ColnewDDdt.DT)) + 1
            setnames(newDDdt.DT, paste0('Qual', (nIDQual + 1):(nQual - 1)), paste0('Qual', (nIDQual + 2):nQual))
            newDDdt.DT[Sort == 'IDDD', (paste0('Qual', nIDQual + 1)) := 'Period']
            newDDdt.DT[Sort != 'IDDD', (paste0('Qual', nQual)) := '']
            newDDdt.DT[Sort != 'IDDD', (paste0('Qual', nIDQual + 1)) := '']
            setcolorder(newDDdt.DT, c('Variable', 'Sort', 'Class', 'Length', paste0('Qual', 1:nQual), 'ValueRegExp'))
        })
        names(DDdt.list) <- DDdtNames.list

        DD$ID <- DDdt.list[['ID']]
        DD$MicroData <- DDdt.list[['MicroData']]
        DD$ParaData <- DDdt.list[['ParaData']]
        DD$Aggregates <- DDdt.list[['Aggregates']]
        DD$AggWeights <- DDdt.list[['AggWeights']]
        DD$Other <- DDdt.list[['Other']]

        namesVNC <- names(DD$VNC)
        for (nameVNC in namesVNC) {

            DD$VNC[[nameVNC]][IDDD != '', Period := '.']
        }

        #setID(DD) <- DDdt.list[['ID']]
        #setMicroData(DD) <- DDdt.list[['MicroData']]
        #setParaData(DD) <- DDdt.list[['ParaData']]
        #setAggregates(DD) <- DDdt.list[['Aggregates']]
        #setAggWeights(DD) <- DDdt.list[['AggWeights']]
        #setOther(DD) <- DDdt.list[['Other']]
        #getVNC(DD)[['MicroData']][IDDD != "", Period := '.', Period]

        IDQual <- getIDQual(DD)
        NonIDQual <- getNonIDQual(DD)
        DataList <- lapply(getData(object), getData)
        Periods <- names(DataList)

        for (Per in Periods) {
            DataList[[Per]][ , Period := Per]
            setcolorder(DataList[[Per]], c(intersect(IDQual, names(DataList[[Per]])),
                                           intersect(NonIDQual, names(DataList[[Per]])),
                                           c('IDDD', 'Value')))
        }

        Datadt <- rbindlist(DataList, fill = TRUE)
        out <- StQ(Data = Datadt, DD = DD)
        return(out)
    }
)
