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
#' # We build a trivial data set:
#' library(data.table)
#'
#' Data1 <- data.table(NumIdEst = c('002', '003', '004'),
#'                     IDDD = c('NewOrders', 'NewOrders', 'NewOrders'),
#'                     Value = c(32150, 12574, 23896))
#' Data1 <- new(Class = 'Datadt', Data1)
#' VNC1 <- data.table(IDQual = c('NumIdEst', ''),
#'                    NonIDQual = c('', ''),
#'                    IDDD = c('', 'NewOrders'),
#'                    NumIdEst = c('', '.'),
#'                    Unit1 = c('numidest', 'cp01'))
#' VNC1 <- new(Class = 'VNCdt', VNC1)
#' VNC1 <- BuildVNC(list(MicroData = VNC1))
#'
#' MicroDD1 <- data.table(Variable = c('NumIdEst', 'NewOrders'),
#'                        Sort = c('IDQual', 'IDDD'),
#'                        Class = c('character', 'numeric'),
#'                        Qual1 = c('', 'NumIdEst'),
#'                        ValueRegExp = c('[0-9]{9}PP', '([0-9]{1, 10}| )'))
#' MicroDD1 <- new(Class = 'DDdt', MicroDD1)
#' DD1 <- new(Class = 'DD', VarNameCorresp = VNC1, MicroData = MicroDD1)
#'
#' # We build the StQ object, and join it with another previously created in a
#'   single object:
#' data(ExampleQ)  
#' Q1 <- new(Class = 'StQ', Data = Data1, DD = DD1)
#' Q2 <- ExampleQ
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

    outputDD <- getDD(e1) + getDD(e2)

    #Incluimos las mismas columnas en ambos objetos
    ColNames.e1 <- copy(names(getData(e1)))
    ColNames.e2 <- copy(names(getData(e2)))
    NewCol.e2 <- setdiff(ColNames.e2, ColNames.e1)
    NewCol.e1 <- setdiff(ColNames.e1, ColNames.e2)
    
    if (length(NewCol.e2) > 0){
        
        Datae1 <- copy(getData(e1))[, NewCol.e2 := character(.N), with = FALSE]
    }
    
    if (length(NewCol.e1) > 0){
        
        Datae2 <- copy(getData(e2))[, NewCol.e1 := character(.N), with = FALSE]
    }else{
        
        Datae2 <- copy(getData(e2))
    }
    

    # Unimos los slots Data con rbindlist eliminando los duplicados
    ColNames <- c(setdiff(names(getData(e1)), c('IDDD', 'Value')), setdiff(names(getData(e2)), setdiff(names(getData(e1)), c('IDDD', 'Value'))))
    setcolorder(Datae1, ColNames)
    setcolorder(Datae2, ColNames)
    output.Data <- rbindlist(list(Datae1, Datae2))
    setkeyv(output.Data, names(output.Data)[-which(names(output.Data) == 'Value')])
    DupRows <- duplicated(output.Data)
    if (sum(DupRows) > 0) {
      cat('[StQ::+] ATENTION!! Duplicated rows! If you are summing data sets
          corresponding to two different time periods, make sure that the time
          reference variable is included as unit qualifier (IDQual).\n\n
          The next rows are duplicated and will be removed:\n\n')
      print(output.Data[DupRows])
    }
    output.Data <- new(Class = 'Datadt', output.Data[!DupRows])

    # Generamos el objeto final
    output <- new(Class = 'StQ', Data = output.Data, DD = outputDD)

    validObject(output)

    return(output)

    }
)
