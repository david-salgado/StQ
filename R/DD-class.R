#' @title S4 class with the dictionary of data (variable specifications)
#'  
#' @description Definition of an S4 class named \code{DD} with the specification 
#' of each variable. 
#' 
#' The class \code{DD} comprises a slot of class \linkS4class{VarNameCorresp} 
#' and slots of class \linkS4class{data.table} of names \code{ID}, 
#' \code{MicroData}, \code{Aggregates}, \code{AggWeights}, \code{Other} each 
#' with at least five columns named \code{Variable}, \code{Sort}, \code{Class}, 
#' \code{Qual1}, and \code{ValueRegExp}. These columns have the following 
#' meanings: 
#' 
#' \itemize{
#'  \item \code{Variable}: Name of the variable. 
#'  \item \code{Sort}: Semantic sort of the variable, which can be a statistical
#'  unit qualifier (\code{IDQual}), a variable (non-unit) qualifier 
#'  (\code{NonIDQual}) and a variable name (\code{IDDD}).
#'  \item \code{Class}: Class of the variable (\code{integer}, \code{numeric},
#'  \code{character}, ...).
#'  \item \code{Qual1}: Name of the variable qualifier 1.
#'  \item \code{ValueRegExp}: regexp of the variable value. 
#' }
#' 
#' This \linkS4class{data.table} is completed with as many columns named 
#' \code{Qual}\emph{j} as necessary in between columns \code{Class} and 
#' \code{ValueRegExp}. 
#' 
#' @slot VarNameCorresp Object of class \linkS4class{VarNameCorresp}.
#' 
#' @slot ID \linkS4class{data.table} with at least the five columns 
#' \code{Variable}, \code{Sort}, \code{Class}, \code{Qual1}, \code{ValueRegExp} 
#' (in that order).
#' 
#' @slot MicroData \linkS4class{data.table} with at least the five columns 
#' \code{Variable}, \code{Sort}, \code{Class}, \code{Qual1}, \code{ValueRegExp} 
#' (in that order).
#'  
#' @slot Aggregates \linkS4class{data.table} with at least the four columns 
#' \code{Variable}, \code{Sort}, \code{Class}, \code{Qual1}, \code{ValueRegExp} 
#' (in that order).
#' 
#' @slot AggWeights \linkS4class{data.table} with at least the four columns 
#' \code{Variable}, \code{Sort}, \code{Class}, \code{Qual1}, \code{ValueRegExp}
#'  (in that order).
#' 
#' @slot Other \linkS4class{data.table} with at least the four columns 
#' \code{Variable}, \code{Sort}, \code{Class}, \code{Qual1}, \code{ValueRegExp}
#'  (in that order).
#' 
#' @examples
#' # An empty DD object is built through the code: 
#' new(Class = 'DD')
#' 
#' # An example:
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                 IDDD = c('', 'Name', 'Surname', 'PostalAddr',
#'                                          'PhoneNo'),
#'                                 NumIdEst = c('', rep('.', 4)),
#'                                 Unit1 = c('numidest', 'nombre', 'apellidos', 
#'                                           'direccion', 'telefono')     
#'                                 ),
#'                 MicroData = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                        NonIDQual = c('', 'IsNatMarket', 
#'                                                      'IsEuroMarket', 
#'                                                      'IsRWMarket',
#'                                                      ''),
#'                                        IDDD = c(rep('', 4), 'NewOrders'),
#'                                        NumIdEst = c(rep('', 4), '.'),
#'                                        IsNatMarket = c(rep('', 4), '0'),
#'                                        IsEuroMarket = c(rep('', 4), '0'),
#'                                        IsRWMarket = c(rep('', 4), '1'),
#'                                        Unit1 = c('numidest', rep('', 3), 'cp09')),
#'                 ParaData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                        NonIDQual = c('', 'Action', ''),
#'                                        IDDD = c(rep('', 2), 'Date'),
#'                                        NumIdEst = c(rep('', 2), '.'),
#'                                        Action = c(rep('', 2), 'Imputation'),
#'                                        Unit1 = c('numidest', '', 'FechaImput')),
#'                 Aggregates = data.table(IDQual = c('Province', 'NACE09', 'IsNatMarket', ''),
#'                                         IDDD = c('', '', '', 'Turnover'),
#'                                         Province = c('', '', '', '.'),
#'                                         NACE09 = c('', '', '', '.'),
#'                                         IsNatMarket = c('', '', '', '1'),
#'                                         Unit1 = c('provincia', 'actividad', '', 'cn01')))
#' VNC <- new(Class = 'VarNameCorresp', VarList)
#' 
#' ### We build the specification data.tables
#' IDdt <- data.table(
#'     Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'     Sort = c('IDQual', rep('IDDD', 4)),
#'     Class = rep('character', 5),
#'     Qual1 = c('', rep('NumIdEst', 4)),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')
#' )
#' Microdt <- data.table(
#'     Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 
#'                  'IsRWMarket', 'NewOrders'),
#'     Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'     Class = c(rep('character', 4), 'numeric'),
#'     Qual1 = c(rep('', 4), 'NumIdEst'),
#'     ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), '([0-9]{1, 10}| )')
#' )
#' Paradt <- data.table(
#'     Variable = c('NumIdEst', 'Action', 'Date'),
#'     Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'     Class = rep('character', 3),
#'     Qual1 = c(rep('', 2), 'NumIdEst'),
#'     Qual2 = c(rep('', 2), 'Action'),
#'     ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation', '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )')
#' )
#' Aggdt <- data.table(
#'     Variable = c('Province', 'NACE09', 'Turnover'),
#'     Sort = c(rep('IDQual', 2), 'IDDD'),
#'     Class = c(rep('character', 2), 'numeric'),
#'     Qual1 = c(rep('', 2), 'Province'),
#'     Qual2 = c(rep('', 2), 'NACE09'),
#'     ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )')
#' )
#' 
#' DD <- new(Class = 'DD', 
#'           VarNameCorresp = VNC, 
#'           ID = IDdt, 
#'           MicroData = Microdt, 
#'           ParaData = Paradt,
#'           Aggregates = Aggdt)
#' 
#' VNC <- getVNC(ExampleDD)
#' DDData <- ExampleDD@@MicroData
#' DDAggregates <- ExampleDD@@Aggregates
#' new(Class = 'DD', VarNameCorresp = VNC, MicroData = DDData, Aggregates = DDAggregates)
#' 
#' @include ExtractNames.R VarNameCorresp-class.R
#' 
#' @import data.table 
#' 
#' @export
setClass(Class = "DD",
         slots = c(VarNameCorresp = 'VarNameCorresp',
                   ID = 'data.table',
                   MicroData = 'data.table',
                   ParaData = 'data.table',
                   Aggregates = 'data.table',
                   AggWeights = 'data.table',
                   Other = 'data.table'),
         prototype = list(VarNameCorresp = new(Class = 'VarNameCorresp'),
                          ID = data.table(Variable = character(0),
                                          Sort = character(0),
                                          Class = character(0),
                                          Qual1 = character(0),
                                          ValueRegExp = character(0)),
                          MicroData = data.table(Variable = character(0),
                                                 Sort = character(0),
                                                 Class = character(0),
                                                 Qual1 = character(0),
                                                 ValueRegExp = character(0)),
                          ParaData = data.table(Variable = character(0),
                                                Sort = character(0),
                                                Class = character(0),
                                                Qual1 = character(0),
                                                ValueRegExp = character(0)),
                          Aggregates = data.table(Variable = character(0),
                                                  Sort = character(0),
                                                  Class = character(0),
                                                  Qual1 = character(0),
                                                  ValueRegExp = character(0)),
                          AggWeights = data.table(Variable = character(0),
                                                  Sort = character(0),
                                                  Class = character(0),
                                                  Qual1 = character(0),
                                                  ValueRegExp = character(0)),
                          Other = data.table(Variable = character(0),
                                             Sort = character(0),
                                             Class = character(0),
                                             Qual1 = character(0),
                                             ValueRegExp = character(0))
                          ),
         validity = function(object){
             
             variablesDD <- c()
             for (Slot in setdiff(slotNames(object), 'VarNameCorresp')) { 
             
                ColNames <- names(slot(object, Slot))
             
                if (ColNames[1] != 'Variable') {
                    stop(paste0('[Validity DD] The first column of slot ', Slot, ' must be named "Variable".'))   
                }
                if (any(duplicated(slot(object, Slot)[['Variable']]))) {
                    stop(paste0('[Validity DD] The column "Variable" of slot ', Slot, ' cannot have repeated values.'))
                }
                
                
                 if (ColNames[2] != 'Sort') {
                     stop(paste0('[Validity DD] The second column of slot ', Slot, ' must be named "Sort".'))
                 }
                 if (length(slot(object, Slot)[['Sort']]) != 0 && 
                     !all(slot(object, Slot)[['Sort']] %in% 
                          c('IDQual', 'NonIDQual', 'IDDD'))) { 
                     stop(paste0('[Validity DD] The column "Sort" of slot ', Slot, ' can only have values "IDQual", "NonIDQual" and "IDDD".'))
                 }
             
                 if (ColNames[3] != 'Class') {
                     stop(paste0('[Validity DD] The third column of slot ', Slot, ' must be "Class".'))
                 }
             
                 if (ColNames[4] != 'Qual1') {
                     stop(paste0('[Validity DD] The fourth column of slot ', Slot, ' must be named "Qual1".'))
                 }
             
                 if (!all(slot(object, Slot)[['Variable']] == ExtractNames(slot(object, Slot)[['Variable']]))) {
                     stop('[Validity DD] There are invalid variable names in the column "Variable".')
                 }
             
                 Quals <- setdiff(ColNames, c('Variable', 'Sort', 'Class', 'ValueRegExp'))
                 if (!all(Quals == paste0('Qual', seq(along = Quals)))) {
                     stop(paste0('[Validity DD] The fourth and succesive columns of slot ', Slot, ' must be named "Qual1", "Qual2", ...'))
                 }
             
                 variablesDD <- c(variablesDD, slot(object, Slot)[Sort == 'IDDD'][['Variable']])
                 variablesDD <- unique(variablesDD)
             }
             
             variablesVNC <- getIDDD(object@VarNameCorresp)
             varVNCnotinDD <- setdiff(variablesVNC, variablesDD) 
             if (length(varVNCnotinDD) > 0) {
                
                     stop(paste0('[Validity DD] The followings variables in the column "IDDD" of the slot VarNameCorresp must be variables ("Sort" = IDDD) in the other slots of the object DD:\n',
                                 paste0(varVNCnotinDD, collapse = ', '),
                                 '\n\n Check if file DD contains all variable names.'))
                 
             }
             return(TRUE)
         }
)
