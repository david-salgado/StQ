#' @title Constructor of objects of class \link{DD}
#'
#' @description This constructor returns an object of class \link{DD}. The input parameter is
#'  a named \code{list} of objects of classes \linkS4class{VarNameCorresp} (named \code{VNC}) and 
#'  \linkS4class{data.table}. Notice that the names of this \code{list} must be, apart from VNC, any
#'   of 'ID', 'MicroData', 'ParaData', 'Aggregates', AggWeights', 'Other'.
#'
#' @param Data A named \code{list} of objects of classes \linkS4class{VarNameCorresp} and 
#' \linkS4class{data.table}.
#'
#' @return An object of class \link{DD} with components specified in the input parameter 
#' \code{Data}. Components 'ID' and/or 'MicroData' not being specified are set as an empty
#' \linkS4class{data.table}.
#' 
#'
#' @examples
#' library(data.table)
#' VarListVNC <- list(
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
#'  Aggregates = data.table(IDQual = c('Province', 'NACE', 'MarketR', ''),
#'                          NonIDQual = rep('', 4),
#'                          IDDD = c('', '', '', 'TotalTurnover'),
#'                          Province = c('', '', '', '.'),
#'                          NACE = c('', '', '', '.'),
#'                          MarketR = c('', '', '', '1.'),
#'                          UnitName = c('provincia', 'actividad', '', 'cn01'),
#'                          InFiles = rep('FP', 4)))
#'                      
#' VNC <- BuildVNC(VarListVNC)
#' 
#' IDdt <- data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                    Sort = c('IDQual', rep('IDDD', 4)),
#'                    Class = rep('character', 5),
#'                    Length = c('11', '15', '15', '20','9'),
#'                    Qual1 = c('', rep('NumIdEst', 4)),
#'                    ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}'))
#' Microdt <- data.table(Variable = c('NumIdEst', 'Market', 'NewOrders'),
#'                       Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                       Class = c(rep('character', 2), 'numeric'),
#'                       Length = c('11', '2', '7'),
#'                       Qual1 = c(rep('', 2), 'NumIdEst'),
#'                       ValueRegExp = c('[0-9]{9}PP', '.+', '([0-9]{1, 10}| )'))
#' BuildDD(Data = list(VNC = VNC, ID = IDdt, MicroData = Microdt))
#' 
#' 
#'
#' @include VNC.R DD.R BuildVNC.R
#'
#' @import data.table
#'
#' @export
BuildDD <- function(Data = list(ID = data.table(Variable = character(0), 
                                                Sort = character(0), 
                                                Class = character(0),
                                                Length = character(0),
                                                Qual1 = character(0),
                                                ValueRegExp = character(0)),
                                MicroData = data.table(Variable = character(0), 
                                                       Sort = character(0), 
                                                       Class = character(0),
                                                       Length = character(0),
                                                       Qual1 = character(0),
                                                       ValueRegExp = character(0)))){
    
    ComponentNames <- names(Data)
    if (is.null(ComponentNames)) stop('[StQ::BuildDD] The input parameter Data must be a named list of data.tables.')
    
    RootCompNames <- unlist(lapply(ComponentNames, 
                                   function(Name){strsplit(Name, split = '_')[[1]][1]}))
    RootCompNames <- unique(RootCompNames)
    if (!'ID' %in% RootCompNames) Data$ID <- data.table(Variable = character(0), 
                                                        Sort = character(0), 
                                                        Class = character(0),
                                                        Length = character(0),
                                                        Qual1 = character(0),
                                                        ValueRegExp = character(0))
    
    if (!'MicroData' %in% RootCompNames) Data$MicroData <- data.table(Variable = character(0), 
                                                                      Sort = character(0), 
                                                                      Class = character(0),
                                                                      Length = character(0),
                                                                      Qual1 = character(0),
                                                                      ValueRegExp = character(0))
    
    if (!'ParaData' %in% RootCompNames) Data$ParaData <- data.table(Variable = character(0), 
                                                                    Sort = character(0), 
                                                                    Class = character(0),
                                                                    Length = character(0),
                                                                    Qual1 = character(0),
                                                                    ValueRegExp = character(0))
    
    IDNames <- names(Data)[grep('ID', names(Data))]
    IDdt <- rbindlist(Data[IDNames], fill = TRUE)
    if (dim(IDdt)[1] == 0) IDdt <- data.table(Variable = character(0), 
                                              Sort = character(0), 
                                              Class = character(0),
                                              Length = character(0),
                                              Qual1 = character(0),
                                              ValueRegExp = character(0))

    MicroDataNames <- names(Data)[grep('MicroData', names(Data))]
    MicroDatadt <- rbindlist(Data[MicroDataNames], fill = TRUE)
    if (dim(MicroDatadt)[1] == 0) MicroDatadt <- data.table(Variable = character(0), 
                                                            Sort = character(0), 
                                                            Class = character(0),
                                                            Length = character(0),
                                                            Qual1 = character(0),
                                                            ValueRegExp = character(0))
    
    ParaDataNames <- names(Data)[grep('ParaData', names(Data))]
    ParaDatadt <- rbindlist(Data[ParaDataNames], fill = TRUE)
    if (dim(ParaDatadt)[1] == 0) ParaDatadt <- data.table(Variable = character(0), 
                                                          Sort = character(0), 
                                                          Class = character(0),
                                                          Length = character(0),
                                                          Qual1 = character(0),
                                                          ValueRegExp = character(0))
    
    AggrDataNames <- names(Data)[grep('Aggregates', names(Data))]
    AggrDatadt <- rbindlist(Data[AggrDataNames], fill = TRUE)
    if (dim(AggrDatadt)[1] == 0) AggrDatadt <- data.table(Variable = character(0), 
                                                          Sort = character(0), 
                                                          Class = character(0),
                                                          Length = character(0),
                                                          Qual1 = character(0),
                                                          ValueRegExp = character(0))
    
    AggWDataNames <- names(Data)[grep('AggWeights', names(Data))]
    AggWDatadt <- rbindlist(Data[AggWDataNames], fill = TRUE)
    if (dim(AggWDatadt)[1] == 0) AggWDatadt <- data.table(Variable = character(0), 
                                                          Sort = character(0), 
                                                          Class = character(0),
                                                          Length = character(0),
                                                          Qual1 = character(0),
                                                          ValueRegExp = character(0))
    OtherDataNames <- names(Data)[grep('Other', names(Data))]
    OtherDatadt <- rbindlist(Data[OtherDataNames], fill = TRUE)
    if (dim(OtherDatadt)[1] == 0) OtherDatadt <- data.table(Variable = character(0), 
                                                            Sort = character(0), 
                                                            Class = character(0),
                                                            Length = character(0),
                                                            Qual1 = character(0),
                                                            ValueRegExp = character(0))
    flag.data <- unique(unlist(lapply(Data, function(DT){dim(DT)[1]})))
    flag.data <- max(flag.data) > 0
    if (!'VNC' %in% RootCompNames & flag.data) stop('[StQ::BuildDD] The input parameter Data must have a component of class VNC.')
    if (!'VNC' %in% RootCompNames & !flag.data) Data$VNC <- VNC()
    
    VarList <- list(VNC = Data[['VNC']], ID = IDdt, MicroData = MicroDatadt, ParaData = ParaDatadt,
                    Aggregates = AggrDatadt, AggWeights = AggWDatadt, Other = OtherDatadt)
    out <- do.call(DD, VarList)
    return(out)

    return(out)
}
