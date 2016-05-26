#' @title Method \code{+} for the class \linkS4class{DDdt}
#'
#' @description \code{+} sums two objects of class \linkS4class{DDdt}. This method
#' overloads the operator \link{+} and returns a new object of class
#' \linkS4class{DDdt}.
#'
#' @param e1 Object of class \linkS4class{DDdt}.
#'
#' @param e2 Object of class \linkS4class{DDdt}.
#'
#' @return Object of class \linkS4class{DDdt} resulting from integrating both
#' \linkS4class{DDdt} objects in a single \linkS4class{DDdt} object.
#'
#' @examples
#' library(data.table)
#' 
#' ID1dt <- new(Class = 'DDdt', 
#'              .Data = data.table(Variable = c('NumIdEst', 'Name', 'Surname', 
#'                                            'PostalAddr', 'PhoneNo'),
#'                                 Sort = c('IDQual', rep('IDDD', 4)),
#'                                 Class = rep('character', 5),
#'                                 Qual1 = c('', rep('NumIdEst', 4)),
#'                                 ValueRegExp = c('[0-9]{9}PP', '.+', '.+', 
#'                                           '.+', '(6|9)[0-9]{8}')))
#' 
#' ID2dt <- new(Class = 'DDdt',
#'              .Data = data.table(Variable = c('NumIdEst', 'Name', 'Surname',
#'                                            'PostalAddr', 'PhoneNo'),
#'                                 Sort = c('IDQual', rep('IDDD', 4)),
#'                                 Class = rep('character', 5),
#'                                 Qual1 = c('', rep('NumIdEst', 4)),
#'                                 ValueRegExp = c('[0-9]{9}PP', '.+', '.+', 
#'                                               '.+', '(6|9)[0-9]{8}')))
#' ID1dt +ID2dt
#' 
#' 
#' Micro1dt <- new(Class = 'DDdt',
#'                 .Data = data.table(Variable = c('NumIdEst', 'IsNatMarket',
#'                                             'IsEuroMarket', 'IsRWMarket',
#'                                              'Turnover'),
#'                                    Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'                                    Class = c(rep('character', 4), 'numeric'),
#'                                    Qual1 = c(rep('', 4), 'NumIdEst'),
#'                                    ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), 
#'                                              '([0-9]{1, 10}| )')))
#' 
#' Micro2dt <- new(Class = 'DDdt',
#'                 .Data = data.table(Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 
#'                                                 'IsRWMarket', 'NewOrders'),
#'                                    Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'                                    Class = c(rep('character', 4), 'numeric'),
#'                                    Qual1 = c(rep('', 4), 'NumIdEst'),
#'                                    ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), 
#'                                                '([0-9]{1, 10}| )')))
#' 
#' Micro1dt + Micro2dt
#' 
#' 
#' Agg1dt <- new(Class = 'DDdt',
#'               .Data = data.table(Variable = c('Province', 'NACE09', 'Turnover'),
#'                                  Sort = c(rep('IDQual', 2), 'IDDD'),
#'                                  Class = c(rep('character', 2), 'numeric'),
#'                                  Qual1 = c(rep('', 2), 'Province'),
#'                                  Qual2 = c(rep('', 2), 'NACE09'),
#'                                  ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])',
#'                                           '([0-9]{1, 15}| )')))  
#' 
#' Agg2dt <- new(Class = 'DDdt',
#'               .Data = data.table(Variable = c('Province', 'NACE09', 'NewOrders'),
#'                                  Sort = c(rep('IDQual', 2), 'IDDD'),
#'                                  Class = c(rep('character', 2), 'numeric'),
#'                                  Qual1 = c(rep('', 2), 'Province'),
#'                                  Qual2 = c(rep('', 2), 'NACE09'),
#'                                  ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])',
#'                                              '([0-9]{1, 15}| )'))) 
#' Agg1dt + Agg2dt
#' 
#'
#' 
#' @include DDdt-class.R 
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "+",
    signature = c("DDdt", "DDdt"),
    definition = function(e1, e2){
        
        CommonCols <- intersect(names(e1), names(e2))
        DDdt1 <- setkeyv(e1, CommonCols)
        DDdt2 <- setkeyv(e2, CommonCols)
        
        outVar <- rbindlist(list(DDdt1, DDdt2), fill = TRUE)
        for (col in names(outVar)) {
            
            outVar[, col := ifelse(is.na(get(col)), '', get(col)), with = F]
            
        }
        
        
        if (dim(outVar)[1] == 0) {
            
            output <- new(Class = 'DDdt')
            
        } else {
            
            QualCol <- names(outVar)[grep('Qual', names(outVar))]
            setkeyv(outVar, setdiff(names(outVar), QualCol))
            outVar <- outVar[!duplicated(outVar)]
            setcolorder(outVar, c('Variable', 'Sort', 'Class', QualCol, 'ValueRegExp'))
            output <- new(Class = 'DDdt', outVar) 
        } 
        
        return(output)

    }
)
