#' @title S4 class for the information contained in DD files
#'  
#' @description Definition of an S4 class named \code{DD} with the information 
#' contained in a DD file with s slightly different structure to that of those
#' files. 
#' 
#' The class \code{DD} comprises a slot of class \linkS4class{data.table} with
#' at least four columns named \code{Variable}, \code{Sort}, \code{Class} and
#' \code{Qual1}. These columns have the same meanings: 
#' 
#' \itemize{
#'  \item \code{Variable}: Name of the variable. 
#'  \item \code{Sort}: Semantic sort of the variable, which can be a statistical
#'  unit qualifier (\code{IDQual}), a variable (non-unit) qualifier 
#'  (\code{NonIDQual}) and a variable name (\code{IDDD}).
#'  \item \code{Class}: Class of the variable (\code{integer}, \code{numeric},
#'  \code{character},...).
#'  \item \code{Qual1}: Name of the variable qualifier 1. 
#' }
#' 
#' This \linkS4class{data.table} is completed with as many columns named 
#' \code{Qualn} as necessary. 
#' 
#' @slot Data \linkS4class{data.table} with at least the four columns 
#' \code{Variable}, \code{Sort}, \code{Class}, \code{Qual1} (in that order).
#'  
#' @examples
#' # An empty DD object is built through the code: 
#' new(Class = 'DD')
#' 
#' # An example with data extracted of a DD object:
#' library(data.table)
#' data(ExampleDD)
#' VNC <- ExampleDD@VarNameCorresp
#' DDData <- ExampleDD@MicroData
#' DDAggregates <- ExampleDD@Aggregates
#' new(Class = 'DD', VarNameCorresp = VNC, MicroData = DDData, Aggregates = DDAggregates)
#' 
#' @include ExtractNames.R VarNameCorresp-class.R
#' 
#' @import data.table 
#' 
#' @export
setClass(Class = "DD",
         slots = c(VarNameCorresp = 'VarNameCorresp',
                   MicroData = 'data.table', 
                   Aggregates = 'data.table',
                   AggWeights = 'data.table',
                   Other = 'data.table'),
         prototype = list(VarNameCorresp = new(Class = 'VarNameCorresp'),
                          MicroData = data.table(Variable = character(0),
                                                 Sort = character(0),
                                                 Class = character(0),
                                                 Qual1 = character(0)),
                          Aggregates = data.table(Variable = character(0),
                                                  Sort = character(0),
                                                  Class = character(0),
                                                  Qual1 = character(0)),
                          AggWeights = data.table(Variable = character(0),
                                                  Sort = character(0),
                                                  Class = character(0),
                                                  Qual1 = character(0)),
                          Other = data.table(Variable = character(0),
                                             Sort = character(0),
                                             Class = character(0),
                                             Qual1 = character(0))
                          ),
         validity = function(object){
             
             variablesDD <- c()
             for (Slot in setdiff(slotNames(object), 'VarNameCorresp')){
             
                ColNames <- names(slot(object, Slot))
             
                if (ColNames[1] != 'Variable'){
                    stop(paste0('[Validity DD] The first column of slot ', Slot, 'must be named "Variable".'))   
                }
                if (any(duplicated(slot(object, Slot)[['Variable']]))){
                    stop(paste0('[Validity DD] The column "Variable" of slot ', Slot, 'cannot have repeated values.'))
                }
                
                #setkeyv(object@Data, 'Variable')
             
                 if (ColNames[2] != 'Sort'){
                     stop(paste0('[Validity DD] The second column of slot ', Slot, 'must be named "Sort".'))
                 }
                 if (length(slot(object, Slot)[['Sort']]) != 0 && 
                     !all(slot(object, Slot)[['Sort']] %in% 
                          c('IDQual', 'NonIDQual', 'IDDD'))){ 
                     stop(paste0('[Validity DD] The column "Sort" of slot ', Slot, 'can only have values "IDQual", "NonIDQual" and "IDDD".'))
                 }
             
                 if (ColNames[3] != 'Class'){
                     stop(paste0('[Validity DD] The third column of slot ', Slot, 'must be "Class".'))
                 }
             
                 if (ColNames[4] != 'Qual1'){
                     stop(paste0('[Validity DD] The fourth column of slot ', Slot, 'must be named "Qual1".'))
                 }
             
                 if (!all(slot(object, Slot)[['Variable']] == ExtractNames(slot(object, Slot)[['Variable']]))){
                     stop('[Validity DD] There are invalid variable names in the column "Variable".')
                 }
             
                 Quals <- setdiff(ColNames, c('Variable', 'Sort', 'Class'))
                 if (!all(Quals == paste0('Qual', seq(along = Quals)))){
                     stop(paste0('[Validity DD] The fourth and succesive columns of slot ', Slot, 'must be named "Qual1", "Qual2", ...'))
                 }
             
                 variablesDD <- c(variablesDD, slot(object, Slot)[Sort == 'IDDD'][['Variable']])
                 variablesDD <- unique(variablesDD)
             }
             
             variablesVNC <- character()
             for (SheetName in object@VarNameCorresp@VarNameCorresp){
                  var <- SheetName[['IDDD']]
                  var <- var[var != ""]
                  variablesVNC <- c(variablesVNC, setdiff(var, variablesVNC))
             }
             if (length(setdiff(variablesVNC, variablesDD)) > 0){
                
                     stop(paste0('[Validity DD] All variables in the column "IDDD" of each element of the slot VarNameCorresp must be variables ("Sort" = IDDD) in the data slots.'))
                 
             }
             return(TRUE)
         }
)
