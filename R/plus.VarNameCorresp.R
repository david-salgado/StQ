#' @title Method \code{+} for the class \linkS4class{VarNameCorresp}
#'
#' @description \code{+} sums two objects of class \linkS4class{VarNameCorresp}.
#'  This method overloads the operator \link{+} and returns a new object of 
#'  class \linkS4class{VarNameCorresp}.
#'
#' The integration is carried out according to the names of the components. 
#'
#' @param e1 Object of class \linkS4class{VarNameCorresp}.
#'
#' @param e2 Object of class \linkS4class{VarNameCorresp}.
#'
#' @return Object of class \linkS4class{VarNameCorresp} resulting from 
#' integrating both \linkS4class{VarNameCorresp} objects in a single 
#' \linkS4class{VarNameCorresp} object.
#'
#' @examples
#' library(data.table)
#' VarList1 <- list(MicroData = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                         NonIDQual = c('', 
#'                                                       'IsNatMarket', 
#'                                                       'IsEuroMarket', 
#'                                                       'IsRWMarket',
#'                                                       ''),
#'                                         IDDD = c(rep('', 4), 'NewOrders'),
#'                                         NumIdEst = rep('', 5),
#'                                         IsNatMarket = c(rep('', 4), '0'),
#'                                         IsEuroMarket = c(rep('', 4), '0'),
#'                                         IsRWMarket = c(rep('', 4), '1'),
#'                                         Unit1 = c(rep('', 4), 'cp09')))
#' VNC1 <- new(Class = 'VarNameCorresp', VarList1)
#' VarList2 <- list(MicroData = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                         NonIDQual = c('', 
#'                                                       'IsNatMarket', 
#'                                                       'IsEuroMarket', 
#'                                                       'IsRWMarket',
#'                                                       ''),
#'                                         IDDD = c(rep('', 4), 'Turnover'),
#'                                         NumIdEst = rep('', 5),
#'                                         IsNatMarket = c(rep('', 4), '0'),
#'                                         IsEuroMarket = c(rep('', 4), '1'),
#'                                         IsRWMarket = c(rep('', 5)),
#'                                         Unit1 = c(rep('', 4), 'cn03')))
#' VNC2 <- new(Class = 'VarNameCorresp', VarList2)
#' VNC1 + VNC2
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "+",
    signature = c("VarNameCorresp", "VarNameCorresp"),
    definition = function(e1, e2){
        
        VNC1Names <- names(e1)
        VNC2Names <- names(e2)
        CommonNames <- intersect(VNC1Names, VNC2Names)
        In1Not2Names <- setdiff(VNC1Names, VNC2Names)
        In2Not1Names <- setdiff(VNC2Names, VNC1Names)
        
        outVarList <- list()
        for (Name in CommonNames) {
            
            CommonCols <- intersect(names(e1[[Name]]), names(e2[[Name]]))
            VNC1 <- setkeyv(e1[[Name]], CommonCols)
            VNC2 <- setkeyv(e2[[Name]], CommonCols)
            outVarList[[Name]] <- rbindlist(list(VNC1, VNC2), fill = TRUE)
            for (col in names(outVarList[[Name]])) {
            
                outVarList[[Name]][, col := ifelse(is.na(get(col)), '', get(col)), with = F]
                
            }
            UnitCol <- names(outVarList[[Name]])[grep('Unit', names(outVarList[[Name]]))]
            setkeyv(outVarList[[Name]], setdiff(names(outVarList[[Name]]), UnitCol))
            outVarList[[Name]] <- outVarList[[Name]][!duplicated(outVarList[[Name]])]
        }

        for (Name in In1Not2Names) {
            
            outVarList[[Name]] <- e1[[Name]]
        }

        for (Name in In2Not1Names) {
            
            outVarList[[Name]] <- e2[[Name]]
        }

        for (Name in names(outVarList)) {
            Unitn <- names(outVarList[[Name]])[grep('Unit', names(outVarList[[Name]]))]
            setcolorder(outVarList[[Name]] , c('IDQual', 'NonIDQual', 'IDDD', setdiff(names(outVarList[[Name]]), c('IDQual', 'NonIDQual', 'IDDD', Unitn)), Unitn))
        }

        output <- new(Class = 'VarNameCorresp', outVarList)
        return(output)
        
    }
)
