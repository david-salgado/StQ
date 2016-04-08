#' @title Method to transform an StQList object into a list of StQ's objects.
#'
#' @description \code{StQListToStQ} transform an StQList object into a list of
#' StQ's objects with a new variable "Period" to take in account the interval
#' times related to StQList object.
#'
#' This method creates a variable with the name \code{Period} in Data slots with
#' the period related to each Data and adds this variable to each DD slot of the
#' new StQ's objects.
#'
#' @param object Object of class \linkS4class{StQList} to be transformed.
#'
#' @return a list of objects of class \linkS4class{StQ}.
#' 
#' @export
setGeneric("StQListToStQ",
           function(object){standardGeneric("StQListToStQ")})

#' @rdname StQListToStQ
#'
#' 
#' @include StQList-class.R StQ-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "StQListToStQ",
    signature = c("StQList"),
    function(object){
        
        DataList <- getData(object)
        Periods <- names(DataList)
        output <- lapply(Periods, function(Per){
            
        DataList[[Per]][, Period := Per]
        setcolorder(DataList[[Per]], c(setdiff(names(DataList[[Per]]), c('IDDD', 'Value')), c('IDDD', 'Value')))
        
        DDList <- getDD(object)
            
        slots <- setdiff(slotNames(DDList[[Per]]), 'VarNameCorresp')
        slotData <- list()
        slotData[['VarNameCorresp']] <- slot(DDList[[Per]], 'VarNameCorresp')
        
        for (slot in slots){
            
            Data <- slot(DDList[[Per]], slot)
            VarNames <- setdiff(names(DataList[[Per]]), c('Period', 'IDDD', 'Value'))
            
            if(length(VarNames[VarNames %in% Data[['Variable']]]) == length(VarNames)){
                
                row <- data.table(Variable = 'Period', Sort = 'NonIDQual', Class = 'character')
                list <- list(Data, row)
                Data <- rbindlist(list, use.names = TRUE, fill = TRUE)
                Data[nrow(Data)] <- ifelse(is.na(Data[nrow(Data)]), '', Data[nrow(Data)])
            }
            
            slotData[[slot]] <- Data
        }
        
        DDList[[Per]] <- new(Class = 'DD', VarNameCorresp = slotData[['VarNameCorresp']],
                                MicroData = slotData[['MicroData']],
                                Aggregates = slotData[['Aggregates']],
                                AggWeights = slotData[['AggWeights']],
                                Other = slotData[['Other']])

        new(Class = 'StQ', Data = DataList[[Per]], DD = DDList[[Per]])
           
        })

        names(output) <- Periods
        return(output)
        
    }
)
