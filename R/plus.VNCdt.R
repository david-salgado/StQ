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
#'                              UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 
#'                                        'telefono'),
#'                              InFiles = rep('FI', 5)))
#'                                            
#' ID2 <- new(Class = 'VNCdt',
#'           .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                              NonIDQual = c(rep('', 5)),
#'                              IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                              NumIdEst = c('', rep('.', 4)),
#'                              UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 
#'                                        'telefono'),
#'                              InFiles = rep('FI', 5)))
#'                                            
#' ID1 + ID2                                            
#'                                            
#' MicroData1 <- new(Class = "VNCdt",
#'                   .Data = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                      NonIDQual = c('', 'Market', ''),
#'                                      IDDD = c(rep('', 2), 'Turnover'),
#'                                      NumIdEst = c(rep('', 2), '.'),
#'                                      Market = c(rep('', 2), '1.'),
#'                                      UnitName = c('numidest', '', 'cn05'),
#'                                      InFiles = rep('FF, FD, FG', 3)))
#'                                      
#' MicroData2 <- new(Class ='VNCdt',
#'                   .Data = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                      NonIDQual = c('', 'Market', ''),
#'                                      IDDD = c(rep('', 2), 'NewOrders'),
#'                                      NumIdEst = c(rep('', 2), '.'),
#'                                      Market = c(rep('', 2), '3.'),
#'                                      UnitName = c('numidest', '', 'cp09'),
#'                                      InFiles = rep('FF, FD, FG', 3)))
#'                                      
#' MicroData1 + MicroData2                                      
#'                                      
#' Aggregates1 <- new(Class = "VNCdt",
#'                    .Data = data.table(IDQual = c('Province', 'NACE', 'Market', ''),
#'                                       NonIDQual = c(rep('', 4)),
#'                                       IDDD = c('', '', '', 'Turnover'),
#'                                       Province = c('', '', '', '.'),
#'                                       NACE = c('', '', '', '.'),
#'                                       Market = c('', '', '', '2.'),
#'                                       UnitName = c('provincia', 'actividad', '', 'cn01'),
#'                                       InFiles = rep('FA', 4)))
#' 
#' 
#' Aggregates2 <- new(Class = 'VNCdt',
#'                   .Data = data.table(IDQual = c('Province', 'NACE', 'Market', ''),
#'                                      NonIDQual = c('', '', '', ''),              
#'                                      IDDD = c('', '', '', 'NewOrders'),
#'                                      Province = c('', '', '', '.'),
#'                                      NACE = c('', '', '', '.'),
#'                                      Market = c('', '', '', '1.'),
#'                                      UnitName = c('provincia', 'actividad', '', 'cp02'),
#'                                      InFiles = rep('FA', 4))) 
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
                
                    outVar[, (col) := ifelse(is.na(get(col)), '', get(col))]
                    
            }
        
        setkeyv(outVar, setdiff(names(outVar), c('UnitName', 'InFiles')))

        outVar <- outVar[!duplicated(outVar, by = key(outVar))]
        IDQual <- unique(outVar[which(IDQual != ""), IDQual])
        NonIDQual <- unique(outVar[which(NonIDQual != ""), NonIDQual])
        NonIDQual <- setdiff(NonIDQual, IDQual)
        setcolorder(outVar, c('IDQual', 'NonIDQual', 'IDDD', IDQual, NonIDQual, 'UnitName', 'InFiles'))
        output <- new(Class = 'VNCdt', outVar) 
        
        return(output)
        
    }
)
