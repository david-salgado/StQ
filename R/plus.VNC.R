#' @title Method \code{+} for the class \linkS4class{VarNameCorresp}
#'
#' @description \code{+} sums two objects of class \linkS4class{VarNameCorresp}. This method
#' overloads the operator \link{+} and returns a new object of class \linkS4class{VarNameCorresp}.
#'
#' The integration is carried out according to the names of the components.
#'
#' @param e1 Object of class \linkS4class{VarNameCorresp}.
#'
#' @param e2 Object of class \linkS4class{VarNameCorresp}.
#'
#' @return Object of class \linkS4class{VarNameCorresp} resulting from integrating both
#' \linkS4class{VarNameCorresp} objects in a single \linkS4class{VarNameCorresp} object.
#'
#' @examples
#' library(data.table)
#' VarList1 <- list(
#'     ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                     NonIDQual = rep('', 5),
#'                     IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                     NumIdEst = c('', rep('.', 4)),
#'                     UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                     InFiles = rep('FI', 5)),
#'     MicroData = data.table(IDQual = c('NumIdEst', '', '', '', '', ''),
#'                            NonIDQual = c('', 'Market', 'Cod','Var1', 'Var2', ''),
#'                            IDDD = c('', '', '', '', '','IEPEntradaPed'),
#'                            NumIdEst = c('', '', '', '', '', '.'),
#'                            Market = c('', '', '', '', '','1.'),
#'                            Cod = c('', '', '', '', '', ''),
#'                            Var1 = c('', '', '', '', '', ''),
#'                            Var2 = c('', '', '', '', '', ''),
#'                            UnitName = c('', '', '', '', '','cp09'),
#'                            InFiles = rep('FF', 6)))
#' VNC1 <- BuildVNC(VarList1)
#'
#' VarList2 <- list(
#'     ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                     NonIDQual = rep('', 5),
#'                     IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                     NumIdEst = c('', rep('.', 4)),
#'                     UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                     InFiles = rep('FI', 5)),
#'     MicroData = data.table(IDQual = c('NumIdEst', '', '', '', '', ''),
#'                            NonIDQual = c('', 'Market', 'Cod','Var1', 'Var2', ''),
#'                            IDDD = c('', '', '', '', '','IEPEntradaPed'),
#'                            NumIdEst = c('', '', '', '', '', '.'),
#'                            Market = c('', '', '', '', '','3.'),
#'                            Cod = c('', '', '', '', '', ''),
#'                            Var1 = c('', '', '', '', '', ''),
#'                            Var2 = c('', '', '', '', '', ''),
#'                            UnitName = c('', '', '', '', '','cp09'),
#'                            InFiles = rep('FF', 6)))
#'
#' VNC2 <- BuildVNC(VarList2)
#'
#' VNC1 + VNC2
#'
#' @include VNC.R BuildVNC.R
#'
#' @import data.table
#'
#' @export
#setMethod(
#    f = "+",
#    signature = c("VNC", "VNC"),
#    definition = function(e1, e2){
`+.VNC` <- function(e1, e2){
        VNC1Names <- names(e1)
        VNC2Names <- names(e2)
        CommonNames <- intersect(VNC1Names, VNC2Names)
        In1Not2Names <- setdiff(VNC1Names, VNC2Names)
        In2Not1Names <- setdiff(VNC2Names, VNC1Names)

        outVarList <- list()

        for (Name in CommonNames){

            #outVarList[[Name]] <- e1[[Name]] + e2[[Name]]
            CommonCols <- intersect(names(e1[[Name]]), names(e2[[Name]]))
            VNCdt1 <- setkeyv(e1[[Name]], CommonCols)
            VNCdt2 <- setkeyv(e2[[Name]], CommonCols)

            outVar <- rbindlist(list(VNCdt1, VNCdt2), fill = TRUE)
            for (col in names(outVar)) {

                outVar[, (col) := ifelse(is.na(get(col)), '', get(col))]

            }

            setkeyv(outVar, setdiff(names(outVar), c('UnitName', 'InFiles')))
            outVar <- outVar[!duplicated(outVar, by = key(outVar), fromLast = TRUE)]
            outVar <- outVar[!duplicated(outVar, by = c('IDQual', 'NonIDQual', 'IDDD', 'UnitName'), fromLast = TRUE)]
            setkeyv(outVar, setdiff(names(outVar), c('UnitName', 'InFiles')))
            IDQual <- unique(outVar[which(IDQual != "")][['IDQual']])
            NonIDQual <- unique(outVar[which(NonIDQual != "")][['NonIDQual']])
            NonIDQual <- setdiff(NonIDQual, IDQual)
            setcolorder(outVar, c('IDQual', 'NonIDQual', 'IDDD', IDQual, NonIDQual, 'UnitName', 'InFiles'))
            outVarList[[Name]] <- outVar

        }

        Restr_e1 <- lapply(In1Not2Names, function(Comp){e1[[Comp]]})
        names(Restr_e1) <- In1Not2Names
        Restr_e2 <- lapply(In2Not1Names, function(Comp){e2[[Comp]]})
        names(Restr_e2) <- In2Not1Names
        outVarList <- c(outVarList, Restr_e1, Restr_e2)
        output <- BuildVNC(outVarList)
        return(output)
}

