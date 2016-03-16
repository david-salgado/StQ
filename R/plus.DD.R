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
#' VNC1List <- list(data.table(IDQual = c('Norden', '', ''),
#'                             NonIDQual = c('', 'CCAA', ''),
#'                             IDDD = c('', '', 'CifraNeg'),
#'                             Norden = c('', '', ''),
#'                             CCAA = c('', '', ''),
#'                             Unit1 = c('', '', '')))
#' names(VNC1List) <- seq(along = VNC1List)
#' VNC1 <- new(Class = 'VarNameCorresp', VarNameCorresp = VNC1List)
#' DD1 <- new(Class = 'DD',
#'            MicroData = data.table(Variable = c('NOrden', 'CCAA', 'CifraNeg'),
#'                              Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                              Class = c('character', 'character', 'numeric'), 
#'                              Qual1 = c('', '', 'NOrden')),
#'            VarNameCorresp = VNC1)
#' VNC2List <- list(data.table(IDQual = c('Norden',''), NonIDQual = c('', ''),
#'                             IDDD = c('', 'logCifraNeg'),
#'                             Norden = c('', ''),
#'                             Unit1 = c('', '')))
#' names(VNC2List) <- seq(along = VNC2List)
#' VNC2 <- new(Class = 'VarNameCorresp', VarNameCorresp = VNC2List)
#' DD2 <- new(Class = 'DD',
#'            MicroData = data.table(Variable = c('Norden', 'logCifraNeg'),
#'                              Sort = c('IDQual', 'IDDD'),
#'                              Class = c('character', 'numeric'),
#'                              Qual1 = c('', 'NOrden')),
#'            VarNameCorresp = VNC2)
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
                      MicroData = output[['MicroData']], 
                      Aggregates = output[['Aggregates']],
                      AggWeights = output[['AggWeights']],
                      Other = output[['Other']],
                      VarNameCorresp = VNCPlus)
        
        return(plusDD)

    }
)
