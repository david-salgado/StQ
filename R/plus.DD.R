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
#' VarList1 <- list(ID = new(Class = "VNCdt",
#'                           .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                              NonIDQual = c(rep('', 5)),
#'                                              IDDD = c('', 'Name', 'Surname', 'PostalAddr', 
#'                                                       'PhoneNo'),
#'                                              NumIdEst = c('', rep('.', 4)),
#'                                              Unit1 = c('numidest', 'nombre', 'apellidos', 
#'                                                 'direccion', 'telefono'))),
#'                  MicroData = new(Class = "VNCdt",
#'                                  .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                                     NonIDQual = c('', 'IsNatMarket', 
#'                                                                   'IsEuroMarket', 
#'                                                                   'IsRWMarket', ''),
#'                                                    IDDD = c(rep('', 4), 'Turnover'),
#'                                                    NumIdEst = c(rep('', 4), '.'),
#'                                                    IsNatMarket = c(rep('', 4), '0'),
#'                                                    IsEuroMarket = c(rep('', 4), '0'),
#'                                                    IsRWMarket = c(rep('', 4), '1'),
#'                                                    Unit1 = c('numidest', rep('', 3), 'cn05'))),
#'                 Aggregates = new(Class = "VNCdt",
#'                                  .Data = data.table(IDQual = c('Province', 'NACE', 'IsNatMarket',
#'                                                                ''),
#'                                                     NonIDQual = c(rep('', 4)),
#'                                                     IDDD = c('', '', '', 'Turnover'),
#'                                                     Province = c('', '', '', '.'),
#'                                                     NACE = c('', '', '', '.'),
#'                                                     IsNatMarket = c('', '', '', '1'),
#'                                                     Unit1 = c('provincia', 'actividad', '', 
#'                                                               'cn01'))))
#' VNC1 <- BuildVNC(VarList1)
#' 
#' ID1dt <- new(Class = 'DDdt', 
#'              .Data = data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 
#'                                              'PhoneNo'),
#'                                 Sort = c('IDQual', rep('IDDD', 4)),
#'                                 Class = rep('character', 5),
#'                                 Qual1 = c('', rep('NumIdEst', 4)),
#'                                 ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', 
#'                                                 '(6|9)[0-9]{8}')))
#' Micro1dt <- new(Class = 'DDdt',
#'                 .Data = data.table(Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 
#'                                                 'IsRWMarket', 'Turnover'),
#'                                    Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'                                    Class = c(rep('character', 4), 'numeric'),
#'                                    Qual1 = c(rep('', 4), 'NumIdEst'),
#'                                    ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), 
#'                                                    '([0-9]{1, 10}| )')))
#' Agg1dt <- new(Class = 'DDdt',
#'               .Data = data.table(Variable = c('Province', 'NACE09', 'Turnover'),
#'                                  Sort = c(rep('IDQual', 2), 'IDDD'),
#'                                  Class = c(rep('character', 2), 'numeric'),
#'                                  Qual1 = c(rep('', 2), 'Province'),
#'                                  Qual2 = c(rep('', 2), 'NACE09'),
#'                                  ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])',
#'                                           '([0-9]{1, 15}| )'))) 
#' 
#' DD1 <- new(Class = 'DD', VarNameCorresp = VNC1, ID = ID1dt, MicroData = Micro1dt, 
#'            Aggregates = Agg1dt)
#' 
#' VarList2 <- list(ID = new(Class = "VNCdt",
#'                           .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                              NonIDQual = c(rep('', 5)),
#'                                              IDDD = c('', 'Name', 'Surname', 'PostalAddr', 
#'                                                       'PhoneNo'),
#'                                              NumIdEst = c('', rep('.', 4)),
#'                                              Unit1 = c('numidest', 'nombre', 'apellidos', 
#'                                                    'direccion', 'telefono'))),     
#'                  MicroData = new(Class = "VNCdt",
#'                                  .Data =data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                                    NonIDQual = c('', 'IsNatMarket', 
#'                                                                  'IsEuroMarket', 
#'                                                          'IsRWMarket', ''),
#'                                                    IDDD = c(rep('', 4), 'NewOrders'),
#'                                                    NumIdEst = c(rep('', 4), '.'),
#'                                                    IsNatMarket = c(rep('', 4), '0'),
#'                                                    IsEuroMarket = c(rep('', 4), '0'),
#'                                                    IsRWMarket = c(rep('', 4), '1'),
#'                                                    Unit1 = c('numidest', rep('', 3), 'cp09'))),
#'                 Aggregates = new(Class = "VNCdt",
#'                                  .Data =data.table(IDQual = c('Province', 'NACE', 'IsNatMarket', 
#'                                                               ''),
#'                                                    NonIDQual = c(rep('', 4)),
#'                                                    IDDD = c('', '', '', 'NewOrders'),
#'                                                    Province = c('', '', '', '.'),
#'                                                    NACE = c('', '', '', '.'),
#'                                                    IsNatMarket = c('', '', '', '1'),
#'                                                    Unit1 = c('provincia', 'actividad','', 
#'                                                              'cp02'))))
#' VNC2 <- BuildVNC(VarList2)
#' 
#' ID2dt <- new(Class = 'DDdt',
#'              .Data = data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 
#'                                              'PhoneNo'),
#'                                 Sort = c('IDQual', rep('IDDD', 4)),
#'                                 Class = rep('character', 5),
#'                                 Qual1 = c('', rep('NumIdEst', 4)),
#'                                 ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', 
#'                                                 '(6|9)[0-9]{8}')))
#' Micro2dt <- new(Class = 'DDdt',
#'                 .Data = data.table(Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 
#'                                                 'IsRWMarket', 'NewOrders'),
#'                                    Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'                                    Class = c(rep('character', 4), 'numeric'),
#'                                    Qual1 = c(rep('', 4), 'NumIdEst'),
#'                                    ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), 
#'                                                '([0-9]{1, 10}| )')))
#' Agg2dt <- new(Class = 'DDdt',
#'               .Data = data.table(Variable = c('Province', 'NACE09', 'NewOrders'),
#'                                  Sort = c(rep('IDQual', 2), 'IDDD'),
#'                                  Class = c(rep('character', 2), 'numeric'),
#'                                  Qual1 = c(rep('', 2), 'Province'),
#'                                  Qual2 = c(rep('', 2), 'NACE09'),
#'                                  ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])',
#'                                              '([0-9]{1, 15}| )'))) 
#' 
#' DD2 <- new(Class = 'DD', VarNameCorresp = VNC2, ID = ID2dt, MicroData = Micro2dt,
#'            Aggregates = Agg2dt)
#' 
#' DD1 + DD2
#'
#' @include DD-class.R plus.DDdt.R plus.VarNameCorresp.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "+",
    signature = c("DD", "DD"),
    definition = function(e1, e2){
        
        DD1slots <- slotNames(e1)
        DD2slots <- slotNames(e2)
        CommonSlots <- intersect(DD1slots, DD2slots)
        In1Not2Names <- setdiff(DD1slots, DD2slots)
        In2Not1Names <- setdiff(DD2slots, DD1slots)
        
        outVarList <- list()
        
        for (Name in CommonSlots) {
          
            outVarList[[Name]] <- slot(e1, Name) + slot(e2, Name)
            
        }
        
        for (Name in In1Not2Names) {
            
            outVarList[[Name]] <- slot(e1, Name)
        }
        
        for (Name in In2Not1Names) {
            
            outVarList[[Name]] <- slot(e2, Name)
        }
      
        output <- new(Class = 'DD',
                      VarNameCorresp = outVarList[['VarNameCorresp']],
                      ID = outVarList[['ID']],
                      MicroData = outVarList[['MicroData']],
                      ParaData = outVarList[['ParaData']],
                      Aggregates = outVarList[['Aggregates']],
                      AggWeights = outVarList[['AggWeights']],
                      Other = outVarList[['Other']])
        return(output)
        
    }
)

