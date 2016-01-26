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
#' VarList1 <- list(MicroData = data.table(IDQual = c('NumIdEst', '', '', '', 
#'                                                    '', ''),
#'                                         NonIDQual = c('', 'EsMercNac', 
#'                                                       'EsMercEuro', 
#'                                                       'EsMercRM',
#'                                                       'Cod',''),
#'                                         IDDD = c('', '', '', '', '', 
#'                                                  'IEPEntradaPed'),
#'                                         NumIdEst = c('', '', '', '', '', ''),
#'                                         EsMercNac = c('', '', '', '', '', '0'),
#'                                         EsMercEuro = c('', '', '', '', '', '0'),
#'                                         EsMercRM = c('', '', '', '', '', '1'),
#'                                         Cod = c('', '', '', '', '', ''),
#'                                         Unit1 = c('', '', '', '', '', 'cp09')))
#' VNC1 <- new(Class = 'VarNameCorresp', VarNameCorresp = VarList1)
#' VarList2 <- list(MicroData = data.table(IDQual = c('NumIdEst', '', '', '', 
#'                                                    '', ''),
#'                                         NonIDQual = c('', 'EsMercNac', 
#'                                                       'EsMercEuro', 
#'                                                       'EsMercRM',
#'                                                       'Cod',''),
#'                                         IDDD = c('', '', '', '', '', 
#'                                                  'IEPEntradaPed'),
#'                                         NumIdEst = c('', '', '', '', '', ''),
#'                                         EsMercNac = c('', '', '', '', '', '0'),
#'                                         EsMercEuro = c('', '', '', '', '', '1'),
#'                                         EsMercRM = c('', '', '', '', '', ''),
#'                                         Cod = c('', '', '', '', '', ''),
#'                                         Unit1 = c('', '', '', '', '', 'cp08')))
#' VNC2 <- new(Class = 'VarNameCorresp', VarNameCorresp = VarList2)
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
        
        VNC1Names <- names(e1@VarNameCorresp)
        VNC2Names <- names(e2@VarNameCorresp)
        CommonNames <- intersect(VNC1Names, VNC2Names)
        In1Not2Names <- setdiff(VNC1Names, VNC2Names)
        In2Not1Names <- setdiff(VNC2Names, VNC1Names)
        
        outVarList <- list()
        for (Name in CommonNames){
            
            CommonCols <- intersect(names(e1@VarNameCorresp[[Name]]),
                                    names(e2@VarNameCorresp[[Name]]))
            setkeyv(e1@VarNameCorresp[[Name]], CommonCols)
            setkeyv(e2@VarNameCorresp[[Name]], CommonCols)
            outVarList[[Name]] <- rbindlist(list(e1@VarNameCorresp[[Name]], 
                                                 e2@VarNameCorresp[[Name]]),
                                            fill = TRUE)
            for (col in names(outVarList[[Name]])){
            
                outVarList[[Name]][, col := ifelse(is.na(get(col)), '', get(col)), with = F]
                
            }
            outVarList[[Name]] <- outVarList[[Name]][!duplicated(outVarList[[Name]])]
            
        }
        for (Name in In1Not2Names){
            
            outVarList[[Name]] <- e1@VarNameCorresp[[Name]]
        }
        
        for (Name in In2Not1Names){
            
            outVarList[[Name]] <- e2@VarNameCorresp[[Name]]
        }
        
        for (Name in names(outVarList)){
            Unitn <- names(outVarList[[Name]])[grep('Unit', names(outVarList[[Name]]))]
            setcolorder(outVarList[[Name]] , c('IDQual', 'NonIDQual', 'IDDD', setdiff(names(outVarList[[Name]]), c('IDQual', 'NonIDQual', 'IDDD', Unitn)), Unitn))
        }

        output <- new(Class = 'VarNameCorresp', VarNameCorresp = outVarList)
        return(output)
        
    }
)
