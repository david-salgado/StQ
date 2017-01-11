#' @title Method \code{+} for the class \linkS4class{DD}
#'
#' @description \code{+} sums two objects of class \linkS4class{DD}. This method overloads the 
#' operator \link{+} and returns a new object of class \linkS4class{DD}.
#'
#' @param e1 Object of class \linkS4class{DD}.
#'
#' @param e2 Object of class \linkS4class{DD}.
#'
#' @return Object of class \linkS4class{DD} resulting from integrating both \linkS4class{DD} objects
#'  in a single \linkS4class{DD} object.
#'
#' @examples
#' library(data.table)
#' VarList1 <- list(ID = new(Class = "VNCdt",
#'                           .Data = data.table(IDQual = c('ID', rep('', 4)),
#'                                              NonIDQual = c(rep('', 5)),
#'                                              IDDD = c('', 'Name', 'Surname', 'PostalAddr', 
#'                                                       'PhoneNo'),
#'                                              ID = c('', rep('.', 4)),
#'                                              UnitName = c('numidest', 'nombre', 'apellidos', 
#'                                                 'direccion', 'telefono'),
#'                                              InFiles = rep('FI', 5))),
#'                  MicroData = new(Class = "VNCdt",
#'                                  .Data = data.table(IDQual = c('ID', rep('', 2)),
#'                                                     NonIDQual = c('', 'Market', ''),
#'                                                     IDDD = c(rep('', 2), 'Turnover'),
#'                                                     ID = c(rep('', 2), '.'),
#'                                                     Market = c(rep('', 2), '1'),
#'                                                     UnitName = c('numidest', '', 'cn05'),
#'                                                     InFiles = rep('FF, FD, FG', 3))),
#'                 Aggregates = new(Class = "VNCdt",
#'                                  .Data = data.table(IDQual = c('Province', 'NACE', 'Market', ''),
#'                                                     NonIDQual = c(rep('', 4)),
#'                                                     IDDD = c('', '', '', 'Turnover'),
#'                                                     Province = c('', '', '', '.'),
#'                                                     NACE = c('', '', '', '.'),
#'                                                     Market = c('', '', '', '2'),
#'                                                     UnitName = c('provincia', 'actividad', '', 
#'                                                               'cn01'),
#'                                                     InFiles = rep('FP', 4))))
#' VNC1 <- BuildVNC(VarList1)
#' 
#' ID1dt <- new(Class = 'DDdt', 
#'              .Data = data.table(Variable = c('ID', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                 Sort = c('IDQual', rep('IDDD', 4)),
#'                                 Class = rep('character', 5),
#'                                 Length = c('11', '25', '25', '50', '9'),
#'                                 Qual1 = c('', rep('ID', 4)),
#'                                 ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', 
#'                                                 '(6|9)[0-9]{8}')))
#' Micro1dt <- new(Class = 'DDdt',
#'                 .Data = data.table(Variable = c('ID', 'Market', 'Turnover'),
#'                                    Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                                    Class = c(rep('character', 2), 'numeric'),
#'                                    Length = c('11', '2', '12'),
#'                                    Qual1 = c('', '', 'ID'),
#'                                    ValueRegExp = c('[0-9]{9}PP', '(0|1| )', '[0-9]{1,12}')))
#' Agg1dt <- new(Class = 'DDdt',
#'               .Data = data.table(Variable = c('Province', 'NACE09', 'Turnover'),
#'                                  Sort = c(rep('IDQual', 2), 'IDDD'),
#'                                  Class = c(rep('character', 2), 'numeric'),
#'                                  Length = c('25', '4', '12'),
#'                                  Qual1 = c(rep('', 2), 'Province'),
#'                                  Qual2 = c(rep('', 2), 'NACE09'),
#'                                  ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])',
#'                                           '([0-9]{1, 15}| )'))) 
#' 
#' DD1 <- new(Class = 'DD', VarNameCorresp = VNC1, ID = ID1dt, MicroData = Micro1dt, 
#'            Aggregates = Agg1dt)
#' 
#' VarList2 <- list(ID = new(Class = "VNCdt",
#'                           .Data = data.table(IDQual = c('ID', rep('', 4)),
#'                                              NonIDQual = c(rep('', 5)),
#'                                              IDDD = c('', 'Name', 'Surname', 'PostalAddr', 
#'                                                       'PhoneNo'),
#'                                              ID = c('', rep('.', 4)),
#'                                              UnitName = c('numidest', 'nombre', 'apellidos', 
#'                                                    'direccion', 'telefono'),
#'                                              InFiles = rep('FI', 5))),     
#'                  MicroData = new(Class = "VNCdt",
#'                                  .Data =data.table(IDQual = c('ID', rep('', 2)),
#'                                                    NonIDQual = c('', 'Market', ''),
#'                                                    IDDD = c(rep('', 2), 'NewOrders'),
#'                                                    ID = c(rep('', 2), '.'),
#'                                                    Market = c(rep('', 2), '1.'),
#'                                                    UnitName = c('numidest', '', 'cp09'),
#'                                                    InFiles = rep('FF, FD, FG', 3))),
#'                 Aggregates = new(Class = "VNCdt",
#'                                  .Data =data.table(IDQual = c('Province', 'NACE', 'Market', ''),
#'                                                    NonIDQual = c(rep('', 4)),
#'                                                    IDDD = c('', '', '', 'NewOrders'),
#'                                                    Province = c('', '', '', '.'),
#'                                                    NACE = c('', '', '', '.'),
#'                                                    Market = c('', '', '', '2.'),
#'                                                    UnitName = c('provincia', 'actividad','', 
#'                                                              'cp02'),
#'                                                    InFiles = rep('FP', 4))))
#' VNC2 <- BuildVNC(VarList2)
#' 
#' ID2dt <- new(Class = 'DDdt',
#'              .Data = data.table(Variable = c('ID', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                 Sort = c('IDQual', rep('IDDD', 4)),
#'                                 Class = rep('character', 5),
#'                                 Length = c('11', '25', '25', '50', '9'),
#'                                 Qual1 = c('', rep('ID', 4)),
#'                                 ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', 
#'                                                 '(6|9)[0-9]{8}')))
#' Micro2dt <- new(Class = 'DDdt',
#'                 .Data = data.table(Variable = c('ID', 'Market', 'NewOrders'),
#'                                    Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                                    Class = c(rep('character', 2), 'numeric'),
#'                                    Length = c('11', '2', '7'),
#'                                    Qual1 = c(rep('', 2), 'ID'),
#'                                    ValueRegExp = c('[0-9]{9}PP', '(0|1| )', 
#'                                                    '([0-9]{1, 10}| )')))
#' Agg2dt <- new(Class = 'DDdt',
#'               .Data = data.table(Variable = c('Province', 'NACE09', 'NewOrders'),
#'                                  Sort = c(rep('IDQual', 2), 'IDDD'),
#'                                  Class = c(rep('character', 2), 'numeric'),
#'                                  Length = c('25', '4', '7'),
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

        for (Name in CommonSlots) {outVarList[[Name]] <- slot(e1, Name) + slot(e2, Name)}

        for (Name in In1Not2Names) {outVarList[[Name]] <- slot(e1, Name)}
        
        for (Name in In2Not1Names) {outVarList[[Name]] <- slot(e2, Name)}

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

