#' @title Constructor of objects of class \linkS4class{VarNameCorresp}
#'
<<<<<<< HEAD
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
=======
#' @description This constructor returns an object of class \linkS4class{VarNameCorresp}.
#' The input parameter is a named \code{list} of objects of class \linkS4class{VNCdt}. Notice that
#' the names of this \code{list} must be any of 'ID', 'MicroData', 'ParaData', 'Aggregates', 
#' 'AggWeights', 'Other'.
#'
#' @param Data A named \code{list} of objects of class \linkS4class{VNCdt}.
#'
#' @return An object of class \linkS4class{VarNameCorresp} with components 
#' specified in the input parameter Data. Components 'ID', 'MicroData' and/or 'ParaData' 
#' not being specified are set as an empty \linkS4class{VNCdt} object.
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' 
#'
#' @examples
#' library(data.table)
#' VarList <- list(
<<<<<<< HEAD
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
=======
#'   ID = new(Class = 'VNCdt', 
#'            .Data = data.table(
#'                  IDQual = c('NumIdEst', rep('', 4)),
#'                  NonIDQual = rep('', 5),
#'                  IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                  NumIdEst = c('', rep('.', 4)),
#'                  UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                  InFiles = rep('FI', 5))),
#'   MicroData = new(Class = 'VNCdt', 
#'                   .Data = data.table(
#'                       IDQual = c('NumIdEst', rep('', 2)),
#'                       NonIDQual = c('', 'Market', ''),
#'                       IDDD = c(rep('', 2), 'NewOrders'),
#'                       NumIdEst = c(rep('', 2), '.'),
#'                       Market = c(rep('', 2), '1.'),
#'                       UnitName = c('numidest', '', 'cp09'),
#'                       InFiles = rep('FF, FD, FG', 3))),
#'  Aggregates = new(Class = 'VNCdt', 
#'                   .Data = data.table(
#'                      IDQual = c('Province', 'NACE', 'Market', ''),
#'                      NonIDQual = rep('', 4),
#'                      IDDD = c('', '', '', 'TotalTurnover'),
#'                      Province = c('', '', '', '.'),
#'                      NACE = c('', '', '', '.'),
#'                      Market = c('', '', '', '1.'),
#'                      UnitName = c('provincia', 'actividad', '', 'cn01'),
#'                      InFiles = rep('FP', 4))))
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'                      
#' VNC <- BuildVNC(VarList)
#' VNC
#' 
#' #Notice that it is indeed an object with complex structure:
#' str(VNC)
#'
<<<<<<< HEAD
#' @include VNC.R
=======
#' @include VarNameCorresp-class.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table
#'
#' @export
<<<<<<< HEAD
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
=======
BuildVNC <- function(Data){
    
    if (is.null(names(Data))) stop('[StQ::BuildVNC] The input parameter Data must be a named list of VNCdt objects.')
    
    if (is.null(Data$ID)) Data$ID <- new(Class = 'VNCdt')
    
    if (is.null(Data$MicroData)) Data$MicroData <- new(Class = 'VNCdt')
    
    ComponentNames <- names(Data)
    RootCompNames <- unlist(lapply(ComponentNames, 
                                   function(Name){
                                       strsplit(Name, '_', fixed = TRUE)[[1]][1]
                                   }))
    RootCompNames <- unique(RootCompNames)
    
    if (!'ParaData' %in% RootCompNames) Data$ParaData <- new(Class = 'VNCdt')
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
    
    ParaDataNames <- names(Data)[grep('ParaData', names(Data))]
    
    Data <- Data[c('ID', 'MicroData', ParaDataNames, setdiff(names(Data), c('ID', 'MicroData', ParaDataNames)))]
<<<<<<< HEAD

    out <- do.call(VNC, Data)
    return(out)    
}


setAs("list", "VNC", function(from) {
    BuildVNC(from)
})
=======
    out <- new(Class = 'VarNameCorresp', .Data = Data)
    validObject(out)
    
    return(out)
}
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
