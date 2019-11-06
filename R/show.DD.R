#' @title Show an object of class \link{DD}
#'
#' @description The method \code{show} shows the slots of an object \link{DD} limiting the
#' number of columns on screen up to 8.
#'
#' It is indeed the method \link[methods]{show} adapted to the class \link{DD}.
#'
#' @param object Object of class \link{DD}.
#'
#' @return Invisible object of class \code{\link{NULL}}.
#'
#' @examples
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                 NonIDQual = c('','','','',''),
#'                                 IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                 NumIdEst = c('', rep('.', 4)),
#'                                 UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                                 InFiles = rep('FI', 5)),
#' MicroData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                        NonIDQual = c('', 'Market', ''),
#'                        IDDD = c(rep('', 2), 'NewOrders'),
#'                        NumIdEst = c(rep('', 2), '.'),
#'                        Market = c(rep('', 2), '1'),
#'                        UnitName = c('numidest', '', 'cp09'),
#'                        InFiles = rep('FF, FD, FG', 3)),
#' ParaData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                       NonIDQual = c('', 'Action', ''),
#'                       IDDD = c(rep('', 2), 'Date'),
#'                       NumIdEst = c(rep('', 2), '.'),
#'                       Action = c(rep('', 2), 'Imputation'),
#'                       UnitName = c('numidest', '', 'FechaImput'),
#'                       InFiles = rep('FP', 3)))
#'
#' VNC <- BuildVNC(VarList)
#'
#' IDdt <- data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                    Sort = c('IDQual', rep('IDDD', 4)),
#'                    Class = rep('character', 5),
#'                    Length = c('11', '15', '15', '20','9'),
#'                    Qual1 = c('', rep('NumIdEst', 4)),
#'                    ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}'))
#' Microdt <- data.table(Variable = c('NumIdEst', 'Market', 'NewOrders'),
#'                       Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                       Class = c(rep('character', 2), 'numeric'),
#'                       Length = c('11', '2', '7'),
#'                       Qual1 = c(rep('', 2), 'NumIdEst'),
#'                       ValueRegExp = c('[0-9]{9}PP', '.+', '([0-9]{1, 10}| )'))
#' Paradt <- data.table(Variable = c('NumIdEst', 'Action', 'Date'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = rep('character', 3),
#'                      Length = c('11', '10', '10'),
#'                      Qual1 = c(rep('', 2), 'NumIdEst'),
#'                      Qual2 = c(rep('', 2), 'Action'),
#'                      ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation',
#'                                      '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )'))
#'
#' DD <- DD(VNC = VNC, ID = IDdt, MicroData = Microdt, ParaData = Paradt)
#' show(DD)
#'
#' @include DD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "show",
    signature = c("DD"),
    function(object){

      for (Slot in setdiff(names(object), 'VNC')){
          
          LocalObject <- object[[Slot]] 
          if (dim(LocalObject)[1] == 0) next
          cat(paste0('Slot ', Slot, '\n\n'))
          
          ColMax <- 8 
          NamesCol <- names(LocalObject)
          NumCol <- length(NamesCol) 
          if (NumCol <= ColMax) {
              
              show(LocalObject)
              
          } else {
              
              NumCols <- min(NumCol, ColMax)
              NamesShowCol <- NamesCol[c(1:(ColMax - 2), (NumCol - 1):NumCol)]
              show(LocalObject[, NamesShowCol, with = F])
              cat('\n\n')
              cat(paste(rep('=', 40)), '\n\n')
              cat(paste0('The following columns have been omitted for clarity:\n ', paste0(setdiff(NamesCol, NamesShowCol), collapse = ', '),'\n'))
              cat(paste(rep('=', 40)), '\n\n')
              return(invisible(NULL))
          }
          
          cat('\n\n')
            
        }
        
        return(invisible(NULL))
    }
)

#' @export
print.DD <- function(object){show(object)}

