#' @title S4 class with the dictionary of data (variable specifications)
#'  
#' @description Definition of an S4 class named \code{DD} with the specification 
#' of each variable. 
#' 
#' The class \code{DD} comprises a slot of class \linkS4class{VarNameCorresp} 
#' and slots of class \linkS4class{DDdt} of names \code{ID}, 
#' \code{MicroData}, \code{Aggregates}, \code{AggWeights}, \code{Other} 
#' 
#' @examples
#' # An empty DD object is built through the code: 
#' new(Class = 'DD')
#' 
#' # An example:
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = new(Class = 'VNCdt',
#'                 data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                            NonIDQual = c('','','','',''),
#'                            IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                            NumIdEst = c('', rep('.', 4)),
#'                            Unit1 = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'))
#'  ),
#' MicroData =new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                            NonIDQual = c('', 'IsNatMarket', 
#'                                                          'IsEuroMarket', 
#'                                                          'IsRWMarket',
#'                                                          ''),
#'                                            IDDD = c(rep('', 4), 'NewOrders'),
#'                                            NumIdEst = c(rep('', 4), '.'),
#'                                            IsNatMarket = c(rep('', 4), '0'),
#'                                            IsEuroMarket = c(rep('', 4), '0'),
#'                                            IsRWMarket = c(rep('', 4), '1'),
#'                                            Unit1 = c('numidest', rep('', 3), 'cp09'))),
#' ParaData = new(Class = 'VNCdt',data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                           NonIDQual = c('', 'Action', ''),
#'                                           IDDD = c(rep('', 2), 'Date'),
#'                                           NumIdEst = c(rep('', 2), '.'),
#'                                           Action = c(rep('', 2), 'Imputation'),
#'                                           Unit1 = c('numidest', '', 'FechaImput'))))
#' 
#' VNC <- new(Class = 'VarNameCorresp', VarList)
#' 
#' IDdt <- new( Class='DDdt',data.table(
#'     Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'     Sort = c('IDQual', rep('IDDD', 4)),
#'     Class = rep('character', 5),
#'     QualOrder = c('1', rep('', 4)),
#'     Qual1 = c('', rep('NumIdEst', 4)),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')))
#' Microdt <- new( Class='DDdt',data.table(
#'     Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 
#'                  'IsRWMarket', 'NewOrders'),
#'     Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'     Class = c(rep('character', 4), 'numeric'),
#'     QualOrder = c('2', '3', '4', '5', ''),
#'     Qual1 = c(rep('', 4), 'NumIdEst'),
#'     ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), '([0-9]{1, 10}| )')))
#' Paradt <-new( Class='DDdt', data.table(
#'     Variable = c('NumIdEst', 'Action', 'Date'),
#'     Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'     Class = rep('character', 3),
#'     QualOrder = c('6', rep('', 2)),
#'     Qual1 = c(rep('', 2), 'NumIdEst'),
#'     Qual2 = c(rep('', 2), 'Action'),
#'     ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation', 
#'                     '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )')))
#' 
#' DD <- new(Class = 'DD', 
#'           VarNameCorresp = VNC, 
#'           ID = IDdt, 
#'           MicroData = Microdt, 
#'           ParaData = Paradt)
#' 
#' @include ExtractNames.R VarNameCorresp-class.R DDdt-class.R
#' 
#' @import data.table 
#' 
#' @export
setClass(Class = "DD",
         slots = c(VarNameCorresp = 'VarNameCorresp',
                   ID = 'DDdt',
                   MicroData = 'DDdt',
                   ParaData = 'DDdt',
                   Aggregates = 'DDdt',
                   AggWeights = 'DDdt',
                   Other = 'DDdt'),
         prototype = list(VarNameCorresp = new(Class = 'VarNameCorresp'),
                          ID = new(Class='DDdt'),
                          MicroData = new(Class='DDdt'),
                          ParaData = new(Class='DDdt'),
                          Aggregates = new(Class='DDdt'),
                          AggWeights = new(Class='DDdt'),
                          Other = new(Class='DDdt')
                          ),
         validity = function(object){
             
             ColNames <- slotNames(object)
             
             if (ColNames[1] != 'VarNameCorresp') {
                 
                 stop('[validity DD] The first column of DD must be VarNameCorresp.')
             }
             
             if (ColNames[2] != 'ID') {
                 
                 stop('[validity DD] The second column of DD must be ID.')
             }
             
             if (ColNames[3] != 'MicroData') {
                 
                 stop('[validity DD] The third column of DD must be MicroData.')
             }
             
             if (ColNames[4] != 'ParaData') {
                 
                 stop('[validity DD] The fourth column of DD must be ParaData.')
             }
             
             if (ColNames[5] != 'Aggregates') {
                 
                 stop('[validity DD] The fifth column of DD must be Aggregates.')
             }
             
             if (ColNames[6] != 'AggWeights') {
                 
                 stop('[validity DD] The last second column of DD must be AggWeights.')
             }
             
             if (ColNames[7] != 'Other') {
                 
                 stop('[validity DD] The last column of DD must be Other.')
             }
             
             variablesDD <- c()
             for (Slot in setdiff(slotNames(object), 'VarNameCorresp')) { 
                 SlotNames <- slot(object, Slot)
                 variablesDD <- c(variablesDD,  SlotNames$Variable[SlotNames$Sort == 'IDDD'])
                 variablesDD <- unique(variablesDD)
             }

             variablesVNC <- getIDDD(object@VarNameCorresp)
             varVNCnotinVNC <- setdiff(variablesDD, variablesVNC) 
            
             if (length(varVNCnotinVNC) > 0) {
                 
                 stop(paste0('[Validity DD] The following variables in the column "IDDD" of the slot DD must be variables (IDDD) in the slot VNC:\n',
                             paste0(varVNCnotinVNC, collapse = ', '),
                             '\n\n Check if object VNC contains all variable names.'))
                 
             }
             
             QualNames <- c()
             for (sl in setdiff(slotNames(object), 'VarNameCorresp')){
                 
                 if (dim(slot(object, sl))[1] == 0) next
                 QualNames <- c(QualNames, slot(object, sl)[['QualOrder']])
                 QualNames <- QualNames[QualNames != '']
                 
             }
             
             if (length(QualNames) != length(unique(QualNames))){
                 
                 stop('[Validity DD] The order of the qualifiers in all slots of the DD object must be unique.')
             }
             

             return(TRUE)
         }
)
