#' @title Method \code{+} for the class \link{DD}
#'
#' @description \code{+} sums two objects of class \link{DD}. This method overloads the 
#' operator \link{+} and returns a new object of class \link{DD}.
#'
#' @param e1 Object of class \link{DD}.
#'
#' @param e2 Object of class \link{DD}.
#'
#' @return Object of class \link{DD} resulting from integrating both \link{DD} objects
#'  in a single \link{DD} object.
#'
#' @examples
#' library(data.table)
#' VarList1 <- list(ID = data.table(IDQual = c('ID', rep('', 4)),
#'                                              NonIDQual = c(rep('', 5)),
#'                                              IDDD = c('', 'Name', 'Surname', 'PostalAddr', 
#'                                                       'PhoneNo'),
#'                                              ID = c('', rep('.', 4)),
#'                                              UnitName = c('numidest', 'nombre', 'apellidos', 
#'                                                 'direccion', 'telefono'),
#'                                              InFiles = rep('FI', 5)),
#'                  MicroData = data.table(IDQual = c('ID', rep('', 2)),
#'                                                     NonIDQual = c('', 'Market', ''),
#'                                                     IDDD = c(rep('', 2), 'Turnover'),
#'                                                     ID = c(rep('', 2), '.'),
#'                                                     Market = c(rep('', 2), '1'),
#'                                                     UnitName = c('numidest', '', 'cn05'),
#'                                                     InFiles = rep('FF, FD, FG', 3)))
#' VNC1 <- BuildVNC(VarList1)
#' 
#' ID1dt <- data.table(Variable = c('ID', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                 Sort = c('IDQual', rep('IDDD', 4)),
#'                                 Class = rep('character', 5),
#'                                 Length = c('11', '25', '25', '50', '9'),
#'                                 Qual1 = c('', rep('ID', 4)),
#'                                 ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', 
#'                                                 '(6|9)[0-9]{8}'))
#' Micro1dt <- data.table(Variable = c('ID', 'Market', 'Turnover'),
#'                                    Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                                    Class = c(rep('character', 2), 'numeric'),
#'                                    Length = c('11', '2', '12'),
#'                                    Qual1 = c('', '', 'ID'),
#'                                    ValueRegExp = c('[0-9]{9}PP', '(0|1| )', '[0-9]{1,12}'))
#' Agg1dt <- data.table(Variable = c('Province', 'NACE09', 'Turnover'),
#'                                  Sort = c(rep('IDQual', 2), 'IDDD'),
#'                                  Class = c(rep('character', 2), 'numeric'),
#'                                  Length = c('25', '4', '12'),
#'                                  Qual1 = c(rep('', 2), 'Province'),
#'                                  Qual2 = c(rep('', 2), 'NACE09'),
#'                                  ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])',
#'                                           '([0-9]{1, 15}| )'))
#' 
#' DD1 <- DD(VNC = VNC1, ID = ID1dt, MicroData = Micro1dt, Aggregates = Agg1dt)
#' 
#' VarList2 <- list(ID = data.table(IDQual = c('ID', rep('', 4)),
#'                                              NonIDQual = c(rep('', 5)),
#'                                              IDDD = c('', 'Name', 'Surname', 'PostalAddr', 
#'                                                       'PhoneNo'),
#'                                              ID = c('', rep('.', 4)),
#'                                              UnitName = c('numidest', 'nombre', 'apellidos', 
#'                                                    'direccion', 'telefono'),
#'                                              InFiles = rep('FI', 5)),     
#'                  MicroData = data.table(IDQual = c('ID', rep('', 2)),
#'                                                    NonIDQual = c('', 'Market', ''),
#'                                                    IDDD = c(rep('', 2), 'NewOrders'),
#'                                                    ID = c(rep('', 2), '.'),
#'                                                    Market = c(rep('', 2), '1.'),
#'                                                    UnitName = c('numidest', '', 'cp09'),
#'                                                    InFiles = rep('FF, FD, FG', 3)))
#' VNC2 <- BuildVNC(VarList2)
#' 
#' ID2dt <- data.table(Variable = c('ID', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                 Sort = c('IDQual', rep('IDDD', 4)),
#'                                 Class = rep('character', 5),
#'                                 Length = c('11', '25', '25', '50', '9'),
#'                                 Qual1 = c('', rep('ID', 4)),
#'                                 ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}'))
#' Micro2dt <- data.table(Variable = c('ID', 'Market', 'NewOrders'),
#'                                    Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                                    Class = c(rep('character', 2), 'numeric'),
#'                                    Length = c('11', '2', '7'),
#'                                    Qual1 = c(rep('', 2), 'ID'),
#'                                    ValueRegExp = c('[0-9]{9}PP', '(0|1| )', 
#'                                                    '([0-9]{1, 10}| )'))
#' Agg2dt <- data.table(Variable = c('Province', 'NACE09', 'NewOrders'),
#'                                  Sort = c(rep('IDQual', 2), 'IDDD'),
#'                                  Class = c(rep('character', 2), 'numeric'),
#'                                  Length = c('25', '4', '7'),
#'                                  Qual1 = c(rep('', 2), 'Province'),
#'                                  Qual2 = c(rep('', 2), 'NACE09'),
#'                                  ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])',
#'                                              '([0-9]{1, 15}| )'))
#' 
#' DD2 <- DD(VNC = VNC2, ID = ID2dt, MicroData = Micro2dt, Aggregates = Agg2dt)
#' 
#' DD1 + DD2
#'
#' @include DD.R plus.DD.R plus.VNC.R
#'
#' @import data.table
#' @export
`+.DD` <- function(e1, e2){        
    
    Variable <- Sort <- NULL
    
    sumDDdt <- function(dt1, dt2){
        
        CommonCols <- intersect(names(dt1), names(dt2))
        DDdt1 <- setkeyv(dt1, CommonCols)
        DDdt2 <- setkeyv(dt2, CommonCols)
        
        outVar <- rbindlist(list(DDdt1, DDdt2), fill = TRUE)
        for (col in names(outVar)){outVar[, (col) := ifelse(is.na(get(col)), '', get(col))]}
        setkeyv(outVar, setdiff(names(outVar), 'ValueRegExp'))
        outVar <- outVar[!duplicated(outVar, by = key(outVar))]
        setkeyv(outVar, 'Variable')
        
        #TechDebt
        dupVars <- outVar[['Variable']][duplicated(outVar[['Variable']])]
        if (length(dupVars) != 0){
            
            dupOutVar <- outVar[Variable %chin% dupVars]
            outVar <- outVar[!data.frame(Variable = dupVars)]
            qualCols <- names(dupOutVar)[grep('Qual', names(dupOutVar))]
            
            for (var in dupVars){
                
                dupOutVar.local <- dupOutVar[Variable == var]
                newOutVar <- copy(dupOutVar.local)[nrow(dupOutVar.local)]
                for (qual in qualCols){
                    
                    quals <- unique(dupOutVar.local[[qual]])
                    quals <- quals[quals != '']
                    same.qual <- (length(quals) <= 1)
                    if (same.qual) {
                        
                        if (length(quals) == 0) quals <- ''
                        newOutVar[, (qual) := quals]
                        
                    } else {
                        
                        setkeyv(outVar, setdiff(names(outVar), 'ValueRegExp'))
                        newOutVar <- dupOutVar.local[!duplicated(dupOutVar.local, fromLast = TRUE, by = key(dupOutVar))]
                        
                    }
                    
                }
                
                outVar <- rbindlist(list(outVar, newOutVar))
                setkeyv(outVar, 'Variable')
            }
            
        }
        ### End TechDebt
        if (sum(duplicated(outVar[Sort == 'IDDD'], by = key(outVar))) > 0) {
            
            stop('[StQ::+.DD] No duplicate IDDD variable allowed.')
            
        }
        outVar <- outVar[!duplicated(outVar, by = key(outVar))]
        if (dim(outVar)[1] == 0) {
            
            output <- data.table(Variable = character(0),
                                 Sort = character(0),
                                 Class = character(0),
                                 Length = character(0),
                                 Qual1 = character(0),
                                 ValueRegExp = character(0))
            
        } else {
            
            QualCol <- names(outVar)[grep('Qual', names(outVar))]
            setkeyv(outVar, setdiff(names(outVar), QualCol))
            outVar <- outVar[!duplicated(outVar, by = key(outVar))]
            setcolorder(outVar, c('Variable', 'Sort', 'Class', 'Length', QualCol, 'ValueRegExp'))
            output <- outVar
        } 
        
        return(output)
    }        
    
    DD1slots <- names(e1)
    DD2slots <- names(e2)
    CommonSlots <- intersect(DD1slots, DD2slots)
    In1Not2Names <- setdiff(DD1slots, DD2slots)
    In2Not1Names <- setdiff(DD2slots, DD1slots)
    
    outVarList <- list()
    
    for (Name in CommonSlots) {
        
        if (Name == 'VNC') {
            
            outVarList[['VNC']] <- e1[[Name]] + e2[[Name]]
            
        } else {
            
            outVarList[[Name]] <- sumDDdt(e1[[Name]], e2[[Name]])
            
        }
    }
    
    
    for (Name in In1Not2Names) {outVarList[[Name]] <- e1[[Name]]}
    
    for (Name in In2Not1Names) {outVarList[[Name]] <- e2[[Name]]}
    
    output <- DD(VNC = outVarList[['VNC']],
                 ID = outVarList[['ID']],
                 MicroData = outVarList[['MicroData']],
                 ParaData = outVarList[['ParaData']],
                 Aggregates = outVarList[['Aggregates']],
                 AggWeights = outVarList[['AggWeights']],
                 Other = outVarList[['Other']])
    return(output)
    
}
