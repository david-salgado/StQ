#' @title Method \code{+} for the class \linkS4class{VNCdt}
#'
#' @description \code{+} sums two objects of class \linkS4class{VNCdt}.
#'  This method overloads the operator \link{+} and returns a new object of 
#'  class \linkS4class{VNCdt}.
#'
#' The integration is carried out according to the names of the components. 
#'
#' @param e1 Object of class \linkS4class{VNCdt}.
#'
#' @param e2 Object of class \linkS4class{VNCdt}.
#'
#' @return Object of class \linkS4class{VNCdt} resulting from 
#' integrating both \linkS4class{VNCdt} objects in a single 
#' \linkS4class{VNCdt} object.
#'
#' @examples
#' library(data.table)
#' 
#' ID1 <- new(Class = "VNCdt", 
#'           .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                              NonIDQual = rep('', 5),
#'                              IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                              NumIdEst = c('', rep('.', 4)),
#'                              Unit1 = c('numidest', 'nombre', 'apellidos', 'direccion', 
#'                                        'telefono')))
#'                                            
#' ID2 <- new(Class = 'VNCdt',
#'           .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                              NonIDQual = c(rep('', 5)),
#'                              IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                              NumIdEst = c('', rep('.', 4)),
#'                              Unit1 = c('numidest', 'nombre', 'apellidos', 'direccion', 
#'                                        'telefono')))
#'                                            
#' ID1 + ID2                                            
#'                                            
#' MicroData1 <- new(Class = "VNCdt",
#'                   .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                      NonIDQual = c('', 'IsNatMarket', 'IsEuroMarket', 
#'                                                    'IsRWMarket', ''),
#'                                      IDDD = c(rep('', 4), 'Turnover'),
#'                                      NumIdEst = c(rep('', 4), '.'),
#'                                      IsNatMarket = c(rep('', 4), '0'),
#'                                      IsEuroMarket = c(rep('', 4), '0'),
#'                                      IsRWMarket = c(rep('', 4), '1'),
#'                                      Unit1 = c('numidest', rep('', 3), 'cn05')))
#'                                      
#' MicroData2 <- new(Class ='VNCdt',
#'                   .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                      NonIDQual = c('', 'IsNatMarket', 'IsEuroMarket', 'IsNew',
#'                                                    ''),
#'                                      IDDD = c(rep('', 4), 'NewOrders'),
#'                                      NumIdEst = c(rep('', 4), '.'),
#'                                      IsNatMarket = c(rep('', 4), '0'),
#'                                      IsEuroMarket = c(rep('', 4), '0'),
#'                                      IsNew = c(rep('', 4), '1'),
#'                                      Unit1 = c('numidest', rep('', 3), 'cp09'),
#'                                      Unit2 = c('norden', rep('', 4))))
#'                                      
#' MicroData1 + MicroData2                                      
#'                                      
#' Aggregates1 <- new(Class = "VNCdt",
#'                    .Data = data.table(IDQual = c('Province', 'NACE', 'IsNatMarket', ''),
#'                                       NonIDQual = c(rep('', 4)),
#'                                       IDDD = c('', '', '', 'Turnover'),
#'                                       Province = c('', '', '', '.'),
#'                                       NACE = c('', '', '', '.'),
#'                                       IsNatMarket = c('', '', '', '1'),
#'                                       Unit1 = c('provincia', 'actividad', '', 'cn01')))
#' 
#' 
#' Aggregates2 <- new(Class = 'VNCdt',
#'                   .Data = data.table(IDQual = c('Province', 'NACE', 'IsNatMarket', ''),
#'                                      NonIDQual = c('', '', '', ''),              
#'                                      IDDD = c('', '', '', 'NewOrders'),
#'                                      Province = c('', '', '', '.'),
#'                                      NACE = c('', '', '', '.'),
#'                                      IsNatMarket = c('', '', '', '1'),
#'                                      Unit1 = c('provincia', 'actividad', '', 'cp02'))) 
#'
#' Aggregates1 + Aggregates2                                                   
#' 
#'
#' @include VNCdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "+",
    signature = c("VNCdt", "VNCdt"),
    definition = function(e1, e2){
        
        CommonCols <- intersect(names(e1), names(e2))
        VNCdt1 <- setkeyv(e1, CommonCols)
        VNCdt2 <- setkeyv(e2, CommonCols)

        outVar <- rbindlist(list(VNCdt1, VNCdt2), fill = TRUE)
        
            for (col in names(outVar)) {
                
                    outVar[, col := ifelse(is.na(get(col)), '', get(col)), with = F]
                    
            }
        
        UnitCol <- names(outVar)[grep('Unit', names(outVar))]
        setkeyv(outVar, setdiff(names(outVar), UnitCol))
        outVar <- outVar[!duplicated(outVar)]

        IDQual <- unique(outVar[which(IDQual != ""), IDQual])
        NonIDQual <- unique(outVar[which(NonIDQual != ""), NonIDQual])
        NonIDQual <- setdiff(NonIDQual, IDQual)
        setcolorder(outVar, c('IDQual', 'NonIDQual', 'IDDD', IDQual, NonIDQual, UnitCol))
        output <- new(Class = 'VNCdt', outVar) 
        
        return(output)
        
    }
)
