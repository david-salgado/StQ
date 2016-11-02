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
#'
#' VarList1 <- list(
#'     ID = new(Class = 'VNCdt',
#'              .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                 NonIDQual = rep('', 5),
#'                                 IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                 NumIdEst = c('', rep('.', 4)),
#'                                 UnitName = c('numidest', 'nombre', 'apellidos', 'direccion',
#'                                           'telefono'),
#'                                 InFiles = rep('FI', 5))),
#'     MicroData = new(Class = 'VNCdt',
#'                     data.table(IDQual = c('NumIdEst', '', '', '', '', ''),
#'                                NonIDQual = c('', 'Market', 'Cod','Var1', 'Var2', ''),
#'                                IDDD = c('', '', '', '', '','IEPEntradaPed'),
#'                                NumIdEst = c('', '', '', '', '', '.'),
#'                                Market = c('', '', '', '', '','1.'),
#'                                Cod = c('', '', '', '', '', ''),
#'                                Var1 = c('', '', '', '', '', ''),
#'                                Var2 = c('', '', '', '', '', ''),
#'                                UnitName = c('', '', '', '', '','cp09'),
#'                                InFiles = rep('FF', 6))),
#'     ParaData = new(Class = 'VNCdt'),
#'     Aggregates = new(Class = 'VNCdt',
#'                      .Data = data.table(
#'                          IDQual = c('Province', 'NACE', 'Market', ''),
#'                          NonIDQual = rep('', 4),
#'                          IDDD = c('', '', '', 'TotalTurnover'),
#'                          Province = c('', '', '', '.'),
#'                          NACE = c('', '', '', '.'),
#'                          Market = c('', '', '', '2.'),
#'                          UnitName = c('provincia', 'actividad', '', 'cn01'),
#'                          InFiles = rep('FP', 4))))
#' VNC1 <- new(Class = 'VarNameCorresp', .Data = VarList1)
#'
#' VarList2 <- list(
#'     ID = new(Class = 'VNCdt',
#'              .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                 NonIDQual = rep('', 5),
#'                                 IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                 NumIdEst = c('', rep('.', 4)),
#'                                 UnitName = c('numidest', 'nombre', 'apellidos', 'direccion',
#'                                           'telefono'),
#'                                 InFiles = rep('FI', 5))),
#'     MicroData = new(Class = 'VNCdt',
#'                     data.table(IDQual = c('NumIdEst', '', '', '', '', ''),
#'                                NonIDQual = c('', 'Market', 'Cod','Var1', 'Var2', ''),
#'                                IDDD = c('', '', '', '', '','IEPEntradaPed'),
#'                                NumIdEst = c('', '', '', '', '', '.'),
#'                                Market = c('', '', '', '', '','3.'),
#'                                Cod = c('', '', '', '', '', ''),
#'                                Var1 = c('', '', '', '', '', ''),
#'                                Var2 = c('', '', '', '', '', ''),
#'                                UnitName = c('', '', '', '', '','cp09'),
#'                                InFiles = rep('FF', 6))),
#'     ParaData = new(Class = 'VNCdt'),
#'     Aggregates = new(Class = 'VNCdt',
#'                      .Data = data.table(
#'                          IDQual = c('Province', 'NACE', 'Market', ''),
#'                          NonIDQual = rep('', 4),
#'                          IDDD = c('', '', '', 'TotalTurnover'),
#'                          Province = c('', '', '', '.'),
#'                          NACE = c('', '', '', '.'),
#'                          Market = c('', '', '', '2.'),
#'                          UnitName = c('provincia', 'actividad', '', 'cn01'),
#'                          InFiles = rep('FA', 4))))
#'
#' VNC2 <- new(Class = 'VarNameCorresp', .Data = VarList2)
#'
#' VNC1 + VNC2
#'
#' @include VarNameCorresp-class.R plus.VNCdt.R
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

            outVarList[[Name]] <- e1[[Name]] + e2[[Name]]

        }

        for (Name in In1Not2Names) {

            outVarList[[Name]] <- e1[[Name]]
        }

        for (Name in In2Not1Names) {

            outVarList[[Name]] <- e2[[Name]]
        }

        output <- new(Class = 'VarNameCorresp', outVarList)
        return(output)

    }
)
