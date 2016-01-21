#' @title Method \code{+} for the class \linkS4class{StQ}
#'
#' @description \code{+} joins two objects of class \linkS4class{StQ} in a
#' single object of the same class.
#'
#' This method overloads the operator \code{\link{+}} and builds a new
#' \linkS4class{StQ} object joining both input objects.
#'
#' @param e1 Object of class \code{StQ}.
#'
#' @param e2 Object of class \code{StQ}.
#'
#' @return Object of class \code{StQ} with the join of both slots \code{DD} and
#' \code{Data}.
#'
#' @examples
#' # We build two trivial data sets:
#' library(data.table)
#' Data1 <- data.table(NOrden = c('ID1', 'ID2'),
#'                     IDDD = c('CifraNeg', 'CifraNeg'),
#'                     Value = c(0, 187364))
#'
#' Data2 <- data.table(NOrden = c('ID1', 'ID2', 'ID1', 'ID2'),
#'                     EsRemuner = c(0, 0, 1, 1),
#'                     IDDD = c('Empleo', 'Empleo', 'Empleo', 'Empleo'),
#'                     Value = c(0, 1, 23, 41))
#'
#' # We build two corresponding DD objects:
#' DDData1 <- data.table(Variable = c('NOrden',
#'                                    'CifraNeg'),
#'                      Sort = c('IDQual',
#'                               'IDDD'),
#'                      Class = c('character',
#'                                'numeric'),
#'                      Qual1 = c('', 'NOrden'))
#' VNC1List <- list(data.table(IDQual = c('Norden', ''),
#'                             NonIDQual = c('', ''),
#'                             IDDD = c('', 'CifraNeg'),
#'                             Norden = c('', ''),
#'                             Unit1 = c('', '')))
#' VNC1 <- new(Class = 'VarNameCorresp', VarNameCorresp = VNC1List)
#' DD1 <- new(Class = 'DD', Data = DDData1, VarNameCorresp = VNC1)
#'
#' DDData2 <- data.table(Variable = c('NOrden',
#'                                    'EsRemuner',
#'                                    'Empleo'),
#'                      Sort = c('IDQual',
#'                               'NonIDQual',
#'                               'IDDD'),
#'                      Class = c('character',
#'                                'integer',
#'                                'integer'),
#'                      Qual1 = c(rep('', 2), 'NOrden'),
#'                      Qual2 = c(rep('', 2), 'EsRemuner'))
#' VNC2List <- list(data.table(IDQual = c('Norden', '', ''),
#'                             NonIDQual = c('', 'EsRemuner', ''),
#'                             IDDD = c('', '', 'Empleo'),
#'                             Norden = c('', '', ''),
#'                             EsRemuner = c('', '', '1'),
#'                             Unit1 = c('', '', '')))
#' VNC2 <- new(Class = 'VarNameCorresp', VarNameCorresp = VNC2List)
#' DD2 <- new(Class = 'DD', Data = DDData2, VarNameCorresp = VNC2)
#'
#' # We build both StQ objects and join them in a single object:
#' Q1 <- new(Class = 'StQ', Data = Data1, DD = DD1)
#' Q2 <- new(Class = 'StQ', Data = Data2, DD = DD2)
#' Q <- Q1 + Q2
#' str(Q)
#'
#' @include StQ-class.R DD-class.R getDD.R getData.R getUnits.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "+",
    signature = c("StQ", "StQ"),
    definition = function(e1, e2){

    output.DD <- getDD(e1) + getDD(e2)

    #Incluimos las mismas columnas en ambos objetos
    ColNames.e1 <- names(getData(e1))
    ColNames.e2 <- names(getData(e2))
    NewCol.e2 <- setdiff(ColNames.e2, ColNames.e1)
    for (NewCol in NewCol.e2){

      e1@Data[, NewCol := '', with = F]

    }
    NewCol.e1 <- setdiff(ColNames.e1, ColNames.e2)
    for (NewCol in NewCol.e1){

      e2@Data[, NewCol := '', with = F]

    }

    # Unimos los slots Data con rbindlist eliminando los duplicados
    ColNames <- c(setdiff(names(getData(e1)), c('IDDD', 'Value')), setdiff(names(getData(e2)), setdiff(names(getData(e1)), c('IDDD', 'Value'))))
    setcolorder(getData(e1), ColNames)
    setcolorder(getData(e2), ColNames)
    output.Data <- rbindlist(list(getData(e1), getData(e2)))
    setkeyv(output.Data, names(output.Data)[-which(names(output.Data) == 'Value')])
    DupRows <- duplicated(output.Data)
    if (sum(DupRows) > 0) {
      cat('[StQ::+] ATENTION!! Duplicated rows! If you are summing data sets
          corresponding to to different time periods, make sure that the time
          reference variable is included as unit qualifier (IDQual).\n\n
          The next rows are duplicated and will be removed:\n\n')
      print(output.Data[DupRows])
    }
    output.Data <- output.Data[!DupRows]

    # Generamos el objeto final
    output <- new(Class = 'StQ', Data = output.Data, DD = output.DD)

    validObject(output)

    return(output)

    }
)
