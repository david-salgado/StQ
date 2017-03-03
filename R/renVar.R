#' @title Rename statistical variable names (IDDD) in an object
#'
#' @description \code{renVar} rename variables in a object keeping the rest of the object intact.
#'
#' This method returns the input object with the specified variable names in the input parameter
#' \code{VarNames} renamed with the names in the input parameter \code{NewVarNames}. Variables are
#' renamed in all slots. Old names are dropped out.
#'
#' @param object Input object.
#'
#' @param VarNames Character vector with the variable names to be renamed.
#'
#' @param NewVarNames Character vector with the new variable names.
#'
#' @return Object with the same class as the input object and all specified variable names duly
#' renamed in all slots.
#'
#' @examples
#' data(ExampleStQ)
#' renVar(ExampleStQ, 'Stocks', 'NewStocks')
#'
#' @export
setGeneric("renVar",
           function(object, VarNames, NewVarNames){standardGeneric("renVar")})
#' @rdname renVar
#'
#' @include StQ-class.R getData.R getDD.R getVNC.R Datadt-class.R DD-class.R DatadtToDT.R
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "renVar",
  signature = c("StQ", "character", "character"),
  function(object, VarNames, NewVarNames){

    if (length(VarNames) == 0) {
      cat('[StQ::renVar] No variable specified. Input object is returned.\n')
      return(object)
    }

    if (length(VarNames) != length(NewVarNames)) {

        stop('[StQ::renVar] VarNames and NewVarNames must be character vectors with the same length.')
    }
    
    outputData <- DatadtToDT(getData(object))
    Data.VarNames <- unique(outputData[['IDDD']])
    NotPresentVar <- setdiff(ExtractNames(VarNames), Data.VarNames)
    if (length(NotPresentVar) != 0) {
      stop(paste0('[StQ::renVar] Variable(s) ',
           paste0(NotPresentVar, collapse = ', '), ' is(are) not in the input object.'))
    }
    auxData <- outputData[['IDDD']]
    for (indexVar in seq(along = VarNames)){
        
        auxData[auxData == VarNames[indexVar]] <- NewVarNames[indexVar]
    }

    outputDD <- list()
    for (DDslot in setdiff(slotNames(getDD(object)), 'VarNameCorresp')){
        outputDD[[DDslot]] <- DatadtToDT(slot(getDD(object), DDslot))
        setkeyv(outputDD[[DDslot]], 'Variable')
        for (indexVar in seq(along = VarNames)){
          
          outputDD[[DDslot]][Variable == VarNames[indexVar], Variable := NewVarNames[indexVar]]
        }
    }
    
    outputDD[['VarNameCorresp']] <- getVNC(object)
    for (VNCSlot in names(outputDD[['VarNameCorresp']])){
        
        for (indexVar in seq(along = VarNames)){
            
            auxDT <- outputDD[['VarNameCorresp']][[VNCSlot]]
            auxDT <- auxDT[IDDD == VarNames[indexVar], IDDD := NewVarNames[indexVar]]
            outputDD[['VarNameCorresp']][[VNCSlot]] <- new(Class = 'VNCdt', auxDT)
        }
    }

    outputData <- new(Class = 'Datadt', outputData[, IDDD := auxData])
    outputDD <- new(Class = 'DD', 
                    VarNameCorresp = outputDD[['VarNameCorresp']],
                    ID = new(Class = 'DDdt', outputDD[['ID']]),
                    MicroData = new(Class = 'DDdt', outputDD[['MicroData']]),
                    ParaData = new(Class = 'DDdt', outputDD[['ParaData']]),
                    Aggregates = new(Class = 'DDdt', outputDD[['Aggregates']]), 
                    AggWeights = new(Class = 'DDdt', outputDD[['AggWeights']]),
                    Other = new(Class = 'DDdt', outputDD[['Other']]))
    output <- new(Class = 'StQ', Data = outputData, DD = outputDD)
    
    return(output)

  }
)
