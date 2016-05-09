#' @title Show an object of class \linkS4class{DD}
#'
#' @description The method \code{show} shows the slots of an object
#' \linkS4class{DD} limiting the number of columns on screen up to 8.
#'
#' It is indeed the method \link[methods]{show} adapted to the class
#' \linkS4class{DD}.
#'
#' @param object Object of class \linkS4class{DD}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'DD'))
#'
#' # A more elaborate example. 
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                                      NonIDQual = c('','','','',''),
#'                                                      IDDD = c('', 'Name', 'Surname', 
#'                                                               'PostalAddr','PhoneNo'),
#'                                                      NumIdEst = c('', rep('.', 4)),
#'                                                      Unit1 = c('numidest', 'nombre', 'apellidos', 
#'                                                                'direccion', 'telefono'))),
#'                 MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                                             NonIDQual = c('', 'IsNatMarket', 
#'                                                                           'IsEuroMarket', 
#'                                                                           'IsRWMarket', ''),
#'                                                             IDDD = c(rep('', 4), 'NewOrders'),
#'                                                             NumIdEst = c(rep('', 4), '.'),
#'                                                             IsNatMarket = c(rep('', 4), '0'),
#'                                                             IsEuroMarket = c(rep('', 4), '0'),
#'                                                             IsRWMarket = c(rep('', 4), '1'),
#'                                                             Unit1 = c('numidest', rep('', 3), 
#'                                                                       'cp09'))),
#'                 ParaData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                                            NonIDQual = c('', 'Action', ''),
#'                                                            IDDD = c(rep('', 2), 'Date'),
#'                                                            NumIdEst = c(rep('', 2), '.'),
#'                                                            Action = c(rep('', 2), 'Imputation'),
#'                                                            Unit1 = c('numidest', '', 
#'                                                                      'FechaImput'))),
#'                 Aggregates = new(Class = 'VNCdt', data.table(IDQual = c('Province', 
#'                                                                         'NACE09', ''),
#'                                                              NonIDQual = rep('', 3),
#'                                                              IDDD = c('', '', 'Turnover'),
#'                                                              Province = c(rep('', 2), '.'),
#'                                                              NACE09 = c(rep('', 2), '.'),
#'                                                              Unit1 = rep('', 3))))
#' 
#' VNC <- new(Class = 'VarNameCorresp', VarList)
#' 
#' ### We build the specification data.tables
#' IDdt <- new(Class='DDdt', data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 
#'                                                   'PhoneNo'),
#'                                      Sort = c('IDQual', rep('IDDD', 4)),
#'                                      Class = rep('character', 5),
#'                                      Qual1 = c('', rep('NumIdEst', 4)),
#'                                      ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', 
#'                                                      '(6|9)[0-9]{8}')))
#' Microdt <- new(Class='DDdt', data.table(Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 
#'                                                      'IsRWMarket', 'NewOrders'),
#'                                         Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'                                         Class = c(rep('character', 5)),
#'                                         Qual1 = c(rep('', 4), 'NumIdEst'),
#'                                         ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), 
#'                                                         '([0-9]{1, 10}| )')))
#' Paradt <- new(Class='DDdt', data.table(Variable = c('NumIdEst', 'Action', 'Date'),
#'                                        Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                                        Class = rep('character', 3),
#'                                        Qual1 = c(rep('', 2), 'NumIdEst'),
#'                                        Qual2 = c(rep('', 2), 'Action'),
#'                                        ValueRegExp = c('[0-9]{9}PP', 
#'                                                        'Collection|Editing|Imputation', 
#'                                                        '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )')
#'              ))
#' Aggdt <- new(Class='DDdt', data.table(Variable = c('Province', 'NACE09', 'Turnover'),
#'                                       Sort = c(rep('IDQual', 2), 'IDDD'),
#'                                       Class = c(rep('character', 3)),
#'                                       Qual1 = c(rep('', 2), 'Province'),
#'                                       Qual2 = c(rep('', 2), 'NACE09'),
#'                                       ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', 
#'                                                       '([0-9]{1, 15}| )')))
#' 
#' DD <- new(Class = 'DD', 
#'           VarNameCorresp = VNC, 
#'           ID = IDdt, 
#'           MicroData = Microdt, 
#'           ParaData = Paradt,
#'           Aggregates = Aggdt)
#'
#' show(DD)
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "show",
    signature = c("DD"),
    function(object){

      for (Slot in slotNames(object)){
          
          cat(paste0('Slot ', Slot, '\n\n'))
          show(slot(object, Slot))
          cat('\n\n')
            
        }
        
        invisible(NULL)
    }
)

