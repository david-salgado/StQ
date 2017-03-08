#' @title Return a reduced \linkS4class{DD} involving the input variable names
#'
#' @description \code{VarNamesToDD} returns a \linkS4class{DD} object including only the variable 
#' names specified in the input parameter \code{VarNames}.
#'
#' @param VarNames Character vector with the compound variable names.
#'
#' @param DD Object of class \linkS4class{DD} with the definition and properties of the variables.
#'
#' @return \linkS4class{DD} involving only the compound variable names specified in the input 
#' parameter \code{VarNames}.
#'
#'
#' @examples
#' VarNames <- c('Turnover')
#' data(ExampleDD)          
#' VarNamesToDD(VarNames, ExampleDD)
#'  
#' VarNames <- c('ID', 'Turnover', 'EmplType', 'Employees')
#' VarNamesToDD(VarNames, ExampleDD)                     
#' 
<<<<<<< HEAD
#' @include ExtractNames.R setVNC.R getVNC.R DD.R getVariables.R setID.R setMicroData.R setAggregates.R setAggWeights.R setOther.R 
=======
#' @include ExtractNames.R setVNC.R getVNC.R DDdt-class.R getVariables.R DatadtToDT.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table
#'
#' @export
VarNamesToDD <- function(VarNames, DD){
    
    NotPresentVar  <- setdiff(ExtractNames(VarNames), getVariables(DD))
    if (length(NotPresentVar) > 0) stop(paste0('[StQ::VarNamesToDD] The following variables are not contained in the DD slot: ', NotPresentVar, '.\n'))
<<<<<<< HEAD
    outputDD <- DD()
=======
    outputDD <- new(Class = 'DD')
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
    setVNC(outputDD) <- getVNC(DD)

    # Para una sola variable
    if (is.character(VarNames) & length(VarNames) == 1) {
<<<<<<< HEAD
               
        DDSlotNames <- setdiff(names(DD), 'VNC')
        for (DDslot in DDSlotNames) {

            DDdtlocal <- DD[[DDslot]]
=======
        
        DDSlotNames <- setdiff(slotNames(DD), 'VarNameCorresp')
        for (DDslot in DDSlotNames) {
            
            DDdtlocal <- slot(DD, DDslot)
            DDdtlocal <- DatadtToDT(DDdtlocal)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            Names.DT <- DDdtlocal[Variable == ExtractNames(VarNames)]

            if (dim(Names.DT)[1] != 0) {
                QualNames <- setdiff(names(Names.DT), 
                                    c('Variable', 'Sort', 'Class', 'Length', 'Qual1', 'ValueRegExp'))
                for (col in QualNames) {
                    
                    if (all(Names.DT[[col]] == '')) Names.DT[, (col) := NULL]
                    
                }
                ColNames.DT <- names(Names.DT)
                nQual <- length(grep('Qual', ColNames.DT)) 
                
                if (nQual >1) {
                    
                    setnames(Names.DT, c('Variable', 'Sort', 'Class', 'Length', paste0('Qual', 1:nQual), 'ValueRegExp'))
                
                } else {
                    
                    setnames(Names.DT, c('Variable', 'Sort', 'Class', 'Length', 'Qual1', 'ValueRegExp'))
                }
            }
            #Construimos el objecto DD por Slots
            if (DDslot == 'ID') {
<<<<<<< HEAD

                setID(outputDD) <- Names.DT

            } else if (DDslot == 'MicroData') {
                
                setMicroData(outputDD) <- Names.DT
                
            } else if (DDslot == 'ParaData') {
                
                setParaData(outputDD) <- Names.DT
                
            } else if (DDslot == 'Aggregates') {
                
                setAggregates(outputDD) <- Names.DT
                
            } else if (DDslot == 'AggWeights') {
                
                setAggWeights(outputDD) <- Names.DT
                
            } else {
                
                setOther(outputDD) <- Names.DT
=======
                
                outputDD@ID <- new(Class = 'DDdt', Names.DT)

            } else if (DDslot == 'MicroData') {
                
                outputDD@MicroData <- new(Class = 'DDdt', Names.DT)
                
                
            } else if (DDslot == 'ParaData') {
                
                outputDD@ParaData <- new(Class = 'DDdt', Names.DT)
                
            } else if (DDslot == 'Aggregates') {
                
                outputDD@Aggregates <- new(Class = 'DDdt', Names.DT)
                
            } else if (DDslot == 'AggWeights') {
                
                outputDD@AggWeights <- new(Class = 'DDdt', Names.DT)
                
            } else {
                
                outputDD@Other <- new(Class = 'DDdt', Names.DT)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
                
            }
            
        }
     
        return(outputDD)
        
    } else {# Ahora para varias variables de entrada
        
        out.list <- lapply(as.list(VarNames), VarNamesToDD, DD = DD)

        out <- Reduce(`+`, out.list, out.list[[1L]])
        
        return(out)
    }
}
