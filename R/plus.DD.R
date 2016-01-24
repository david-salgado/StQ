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
#'                              Qual1 = c('', 'NOrden')), VarNameCorresp = VNC2)
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
        
        #namesVNC1 <- names(e1@VarNameCorresp@VarNameCorresp)
        #namesVNC2 <- names(e2@VarNameCorresp@VarNameCorresp)
        #CommonSheets <- as.list(intersect(namesVNC1, namesVNC2))
            
        #VNCListPlus <- lapply(CommonSheets, function(SheetName){
        #        
        #    SheetName1 <- e1@VarNameCorresp@VarNameCorresp
        #    if (SheetName %in% names(SheetName1)){
        #            
        #        SheetName1 <- SheetName1[[names(SheetName1) == SheetName]]
        #        SheetName2 <- e2@VarNameCorresp@VarNameCorresp
        #        SheetName2 <- SheetName2[[names(SheetName2) == SheetName]]
        #            
        #        colSheet1 <- names(SheetName1)
        #        colSheet2 <- names(SheetName2)
        #        CommonSheetCol <- intersect(colSheet1, colSheet2)
        #        colSheet1NotSheet2 <- setdiff(colSheet1, colSheet2)
        #        if (length(colSheet1NotSheet2) > 0) {
        #                
        #            for (NewCol in colSheet1NotSheet2){
        #                
        #                SheetName2[, NewCol := '', with = F]
        #                    
        #            }
        #        }
        #        colSheet2NotSheet1 <- setdiff(colSheet2, colSheet1)
        #        if (length(colSheet2NotSheet1) > 0) {
        #                
        #            for (NewCol in colSheet2NotSheet1){
        #                    
        #                SheetName1[, NewCol := '', with = F]
        #                    
        #            }
        #        }
        #        output <- rbindlist(list(SheetName1, SheetName2))
        #        output <- output[!duplicated(output)]
        #        setkeyv(output, names(output))
        #        setorderv(output, names(output), rep(-1, length(output)))
        #        columns <- names(output)
        #        ColIDQual <- output[[columns[1]]]
        #        ColIDQual <- ColIDQual[ColIDQual!=""]
        #        ColIDNonQual <- output[[columns[2]]]
        #        ColIDNonQual <- ColIDNonQual[ColIDNonQual!=""]
        #        colAdd <- c(ColIDQual, ColIDNonQual)
        #        colorder <- c(columns[1:3], colAdd)
        #        colorder <- c(colorder, setdiff(columns, colorder))
        #        setcolorder(output, colorder)
        #    }
        #})
        #names(VNCListPlus) <- CommonSheets
        #VNCPlus <- new(Class = 'VarNameCorresp', VarNameCorresp = VNCListPlus)
        
        plusDD <- new(Class = 'DD', 
                      MicroData = output[['MicroData']], 
                      Aggregates = output[['Aggregates']],
                      AggWeights = output[['AggWeights']],
                      Other = output[['Other']],
                      VarNameCorresp = VNCPlus)
        
        return(plusDD)

    }
)
