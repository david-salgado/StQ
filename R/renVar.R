#' @title Rename statistical variable names (IDDD) in an object
#'
#' @description \code{renVar} rename variables in a object keeping the rest of the object intact.
#'
#' This method returns the input object with the specified variable names in the input parameter
#' \code{VarNames} renamed with the names in the input parameter \code{NewVarNames}. Variables are
#' renamed in all slots. Old names are dropped out.
#'
#' @param object Input object of class \link{StQ}.
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
#' @include StQ.R getData.R getDD.R getVNC.R DD.R
#'
#' @export
setGeneric("renVar",
           function(object, VarNames, NewVarNames){standardGeneric("renVar")})
#' @rdname renVar
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
    
    outputData <- getData(object)
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
    DD <- getDD(object)
    for (DDslot in setdiff(names(DD), 'VNC')){
        outputDD[[DDslot]] <- DD[[DDslot]]
        setkeyv(outputDD[[DDslot]], 'Variable')
        for (indexVar in seq(along = VarNames)){
          
          outputDD[[DDslot]][Variable == VarNames[indexVar], Variable := NewVarNames[indexVar]]
        }
    }
    
    outputDD[['VNC']] <- getVNC(object)
    for (VNCSlot in names(outputDD[['VNC']])){
        
        for (indexVar in seq(along = VarNames)){
            
            auxDT <- outputDD[['VNC']][[VNCSlot]]
            auxDT <- auxDT[IDDD == VarNames[indexVar], IDDD := NewVarNames[indexVar]]
        }
    }

    outputData <- outputData[, IDDD := auxData]
    outputDD <- DD(VNC = outputDD[['VNC']],
                   ID = outputDD[['ID']],
                   MicroData = outputDD[['MicroData']],
                   ParaData = outputDD[['ParaData']],
                   Aggregates = outputDD[['Aggregates']], 
                   AggWeights = outputDD[['AggWeights']],
                   Other = outputDD[['Other']])
    output <- StQ(Data = outputData, DD = outputDD)
    
    return(output)

  }
)
