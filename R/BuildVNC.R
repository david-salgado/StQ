#' @title Constructor of objects of class \linkS4class{VarNameCorresp}
#'
#' @description This constructor returns an object of class \linkS4class{VNC}.
#' The input parameter is a named \code{list} of objects of class \linkS4class{data.table}. Notice 
#' that the names of this \code{list} must be any of 'ID', 'MicroData', 'ParaData', 'Aggregates', 
#' 'AggWeights', 'Other' or compound names with these roots (e.g. ParaData_Editing).
#'
#' @param Data A named \code{list} of objects of class \linkS4class{data.table}.
#'
#' @return An object of class \linkS4class{VarNameCorresp} with components specified in the input 
#' parameter Data. Components 'ID', 'MicroData' and/or 'ParaData' not being specified are set as an 
#' empty \linkS4class{data.table}.
#' 
#'
#' @examples
#' library(data.table)
#' VarList <- list(
#'   ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                   NonIDQual = rep('', 5),
#'                   IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                   NumIdEst = c('', rep('.', 4)),
#'                   UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                   InFiles = rep('FI', 5)),
#'   MicroData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                          NonIDQual = c('', 'Market', ''),
#'                          IDDD = c(rep('', 2), 'NewOrders'),
#'                          NumIdEst = c(rep('', 2), '.'),
#'                          Market = c(rep('', 2), '1.'),
#'                          UnitName = c('numidest', '', 'cp09'),
#'                          InFiles = rep('FF, FD, FG', 3)),
#'  Aggregates = data.table(IDQual = c('Province', 'NACE', 'Market', ''),
#'                          NonIDQual = rep('', 4),
#'                          IDDD = c('', '', '', 'TotalTurnover'),
#'                          Province = c('', '', '', '.'),
#'                          NACE = c('', '', '', '.'),
#'                          Market = c('', '', '', '1.'),
#'                          UnitName = c('provincia', 'actividad', '', 'cn01'),
#'                          InFiles = rep('FP', 4)))
#'                      
#' VNC <- BuildVNC(VarList)
#' VNC
#' 
#' #Notice that it is indeed an object with complex structure:
#' str(VNC)
#'
#' @include VNC.R
#'
#' @import data.table
#'
#' @export
BuildVNC <- function(Data = list(ID = data.table(IDQual = character(0),
                                                 NonIDQual = character(0),
                                                 IDDD = character(0),
                                                 UnitName = character(0),
                                                 InFiles = character(0)),
                                 MicroData = data.table(IDQual = character(0),
                                                        NonIDQual = character(0),
                                                        IDDD = character(0),
                                                        UnitName = character(0),
                                                        InFiles = character(0)))){
    
    ComponentNames <- names(Data)
    if (is.null(ComponentNames)) stop('[StQ::BuildVNC] The input parameter Data must be a named list of data.tables.')
    
    RootCompNames <- unlist(lapply(ComponentNames, 
                                   function(Name){strsplit(Name, split = '_')[[1]][1]}))
    RootCompNames <- unique(RootCompNames)
    if (!'ID' %in% RootCompNames) Data$ID <- data.table(IDQual = character(0), 
                                                        NonIDQual = character(0), 
                                                        IDDD = character(0),
                                                        UnitName = character(0),
                                                        InFiles = character(0))
    
    if (!'MicroData' %in% RootCompNames) Data$MicroData <- data.table(IDQual = character(0), 
                                                                     NonIDQual = character(0), 
                                                                     IDDD = character(0),
                                                                     UnitName = character(0),
                                                                     InFiles = character(0))
    if (!'ParaData' %in% RootCompNames) Data$ParaData <- data.table(IDQual = character(0), 
                                                                    NonIDQual = character(0), 
                                                                    IDDD = character(0),
                                                                    UnitName = character(0),
                                                                    InFiles = character(0))
    
    ParaDataNames <- names(Data)[grep('ParaData', names(Data))]
    
    Data <- Data[c('ID', 'MicroData', ParaDataNames, setdiff(names(Data), c('ID', 'MicroData', ParaDataNames)))]

    out <- do.call(VNC, Data)
    return(out)    
}


setAs("list", "VNC", function(from) {
    BuildVNC(from)
})
