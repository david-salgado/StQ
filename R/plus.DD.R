#' @title Method \code{+} for the class \linkS4class{DD}
#'
#' @description \code{+} sums two objects of class \linkS4class{DD}. This method
#' overloads the operator \link{+} and returns a new object of class
#' \linkS4class{DD}.
#'
#' @param e1 Object of class \linkS4class{DD}.
#'
#' @param e2 Object of class \linkS4class{DD}.
#'
#' @return Object of class \linkS4class{DD} resulting from integrating both
#' \linkS4class{DD} objects in a single \linkS4class{DD} object.
#'
#' @examples
#' library(data.table)
#' VarList1 <- list(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                  IDDD = c('', 'Name', 'Surname', 
#'                                           'PostalAddr', 'PhoneNo'),
#'                                  NumIdEst = c('', rep('.', 4)),
#'                                  Unit1 = c('numidest', 'nombre', 'apellidos', 
#'                                            'direccion', 'telefono')     
#'                                 ),
#'                  MicroData = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                         NonIDQual = c('', 'IsNatMarket', 
#'                                                       'IsEuroMarket', 
#'                                                       'IsRWMarket',
#'                                                       ''),
#'                                         IDDD = c(rep('', 4), 'Turnover'),
#'                                         NumIdEst = c(rep('', 4), '.'),
#'                                         IsNatMarket = c(rep('', 4), '0'),
#'                                         IsEuroMarket = c(rep('', 4), '0'),
#'                                         IsRWMarket = c(rep('', 4), '1'),
#'                                         Unit1 = c('numidest', rep('', 3), 'cn05')),
#'                 Aggregates = data.table(IDQual = c('Province', 'NACE', 
#'                                                    'IsNatMarket', ''),
#'                                         IDDD = c('', '', '', 'Turnover'),
#'                                         Province = c('', '', '', '.'),
#'                                         NACE = c('', '', '', '.'),
#'                                         IsNatMarket = c('', '', '', '1'),
#'                                         Unit1 = c('provincia', 'actividad', '', 'cn01')))
#' VNC1 <- new(Class = 'VarNameCorresp', VarList1)
#' 
#' ID1dt <- data.table(
#'     Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'     Sort = c('IDQual', rep('IDDD', 4)),
#'     Class = rep('character', 5),
#'     Qual1 = c('', rep('NumIdEst', 4)),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')
#' )
#' Micro1dt <- data.table(
#'     Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 
#'                  'IsRWMarket', 'Turnover'),
#'     Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'     Class = c(rep('character', 4), 'numeric'),
#'     Qual1 = c(rep('', 4), 'NumIdEst'),
#'     ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), '([0-9]{1, 10}| )')
#' )
#' Agg1dt <- data.table(
#'     Variable = c('Province', 'NACE09', 'Turnover'),
#'     Sort = c(rep('IDQual', 2), 'IDDD'),
#'     Class = c(rep('character', 2), 'numeric'),
#'     Qual1 = c(rep('', 2), 'Province'),
#'     Qual2 = c(rep('', 2), 'NACE09'),
#'     ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )')
#' )
#' 
#' DD1 <- new(Class = 'DD', VarNameCorresp = VNC1, ID = ID1dt, MicroData = Micro1dt, Aggregates = Agg1dt)
#' 
#' VarList2 <- list(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                  IDDD = c('', 'Name', 'Surname', 
#'                                           'PostalAddr', 'PhoneNo'),
#'                                  NumIdEst = c('', rep('.', 4)),
#'                                  Unit1 = c('numidest', 'nombre', 'apellidos', 
#'                                            'direccion', 'telefono')     
#'                                 ),
#'                  MicroData = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                         NonIDQual = c('', 'IsNatMarket', 
#'                                                       'IsEuroMarket', 
#'                                                       'IsRWMarket',
#'                                                       ''),
#'                                         IDDD = c(rep('', 4), 'NewOrders'),
#'                                         NumIdEst = c(rep('', 4), '.'),
#'                                         IsNatMarket = c(rep('', 4), '0'),
#'                                         IsEuroMarket = c(rep('', 4), '0'),
#'                                         IsRWMarket = c(rep('', 4), '1'),
#'                                         Unit1 = c('numidest', rep('', 3), 'cp09')),
#'                 Aggregates = data.table(IDQual = c('Province', 'NACE', 
#'                                                    'IsNatMarket', ''),
#'                                         IDDD = c('', '', '', 'NewOrders'),
#'                                         Province = c('', '', '', '.'),
#'                                         NACE = c('', '', '', '.'),
#'                                         IsNatMarket = c('', '', '', '1'),
#'                                         Unit1 = c('provincia', 'actividad', '', 'cp02')))
#' VNC2 <- new(Class = 'VarNameCorresp', VarList2)
#' 
#' ID2dt <- data.table(
#'     Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'     Sort = c('IDQual', rep('IDDD', 4)),
#'     Class = rep('character', 5),
#'     Qual1 = c('', rep('NumIdEst', 4)),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')
#' )
#' Micro2dt <- data.table(
#'     Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 
#'                  'IsRWMarket', 'NewOrders'),
#'     Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'     Class = c(rep('character', 4), 'numeric'),
#'     Qual1 = c(rep('', 4), 'NumIdEst'),
#'     ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), '([0-9]{1, 10}| )')
#' )
#' Agg2dt <- data.table(
#'     Variable = c('Province', 'NACE09', 'NewOrders'),
#'     Sort = c(rep('IDQual', 2), 'IDDD'),
#'     Class = c(rep('character', 2), 'numeric'),
#'     Qual1 = c(rep('', 2), 'Province'),
#'     Qual2 = c(rep('', 2), 'NACE09'),
#'     ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )')
#' )
#' 
#' DD2 <- new(Class = 'DD', VarNameCorresp = VNC2, ID = ID2dt, MicroData = Micro2dt, Aggregates = Agg2dt)
#' 
#' DD1 + DD2
#'
#' @include DD-class.R getData.R getVNC.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "+",
    signature = c("DD", "DD"),
    definition = function(e1, e2){
        
        output <- list()
        DDSlotNames <- setdiff(slotNames(e1), 'VarNameCorresp')
        for (DDslot in DDSlotNames){
            
            Data1 <- slot(e1, DDslot)
            colNames1 <- names(Data1)
    
            Data2 <- slot(e2, DDslot)
            colNames2 <- names(Data2)
    
            CommonCol <- intersect(colNames1, colNames2)
            col1Not2 <- setdiff(colNames1, colNames2)
            if (length(col1Not2) > 0) {
    
                for (NewCol in col1Not2){
    
                    Data2[, NewCol := '', with = F]
    
                }
            }
            col2Not1 <- setdiff(colNames2, colNames1)
            if (length(col2Not1) > 0) {
    
                for (NewCol in col2Not1){
    
                    Data1[, NewCol := '', with = F]
    
                }
            }
    
            DataPlus <- rbindlist(list(Data1, Data2))
            if (is.null(DataPlus)) {
                
                DataPlus <- data.table(Variable = character(0),
                                       Sort = character(0),
                                       Class = character(0),
                                       Qual1 = character(0))
            } else {
                setkeyv(DataPlus, names(DataPlus))
                DataPlus <- DataPlus[!duplicated(DataPlus)]
                output[[DDslot]] <- DataPlus   
        
            }
        }
        
        VNCPlus <- getVNC(e1) + getVNC(e2)

        plusDD <- new(Class = 'DD', 
                      ID = output[['ID']],
                      MicroData = output[['MicroData']], 
                      Aggregates = output[['Aggregates']],
                      AggWeights = output[['AggWeights']],
                      Other = output[['Other']],
                      VarNameCorresp = VNCPlus)
        
        return(plusDD)

    }
)
