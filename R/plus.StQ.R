#' @title Method \code{+} for the class \linkS4class{StQ}
#'
#' @description \code{+} joins two objects of class \linkS4class{StQ} in a single object of the same
#'  class.
#'
#' This method overloads the operator \code{\link{+}} and builds a new \linkS4class{StQ} object 
#' combining both input objects.
#'
#' @param e1 Object of class \code{StQ}.
#'
#' @param e2 Object of class \code{StQ}.
#'
#' @return Object of class \code{StQ} with the join of both slots \code{DD} and
#' \code{Data}.
#' 
#'
#' @examples
#' # We build a trivial data set:
#' library(data.table)
#'
#' Data1 <- data.table(ID = c('002', '003', '004'),
#'                     IDDD = c('NewOrders', 'NewOrders', 'NewOrders'),
#'                     Value = c(32150, 12574, 23896))
#' VNC1 <- data.table(IDQual = c('ID', ''),
#'                    NonIDQual = c('', ''),
#'                    IDDD = c('', 'NewOrders'),
#'                    ID = c('', '.'),
#'                    UnitName = c('numidest', 'cp01'),
#'                    InFiles = rep('FF', 2))
#' VNC1 <- BuildVNC(list(MicroData = VNC1))
#'
#' MicroDD1 <- data.table(Variable = c('ID', 'NewOrders'),
#'                        Sort = c('IDQual', 'IDDD'),
#'                        Class = c('character', 'numeric'),
#'                        Length = c('11', '7'),
#'                        Qual1 = c('', 'ID'),
#'                        ValueRegExp = c('[0-9]{9}PP', '([0-9]{1, 10}| )'))
#' DD1 <- DD(VNC = VNC1, MicroData = MicroDD1)
#'
#' # We build the StQ object, and join it with another previously created in a
#' #  single object:
#' data(ExampleStQ)  
#' Q1 <- StQ(Data = Data1, DD = DD1)
#' Q2 <- ExampleStQ
#' Q1 + Q2
#'
#'
#' @include StQ.R DD.R getDD.R getData.R getUnits.R
#'
#' @import data.table
#'
#' @export
`+.StQ` <- function(e1, e2){        
    
    outputDD <- getDD(e1) + getDD(e2)

    #Incluimos las mismas columnas en ambos objetos
    e1.Data <- getData(e1)
    e2.Data <- getData(e2)
    
    ColNames.e1 <- names(e1.Data)
    ColNames.e2 <- names(e2.Data)
    NewCol.e2 <- setdiff(ColNames.e2, ColNames.e1)
    NewCol.e1 <- setdiff(ColNames.e1, ColNames.e2)
    
  
    if (length(NewCol.e2) > 0){
        
        Datae1 <- e1.Data[, (NewCol.e2) := character(.N)]
        
    } else {
        
        Datae1 <- e1.Data

    }

    if (length(NewCol.e1) > 0){

        Datae2 <- e2.Data[, (NewCol.e1) := character(.N)]

    } else {
        
        Datae2 <- e2.Data
    }
    
    # Unimos los slots Data con rbindlist eliminando los duplicados
    ColNames <- c(setdiff(names(e1.Data), c('IDDD', 'Value')), 
                  setdiff(names(e2.Data), setdiff(names(e1.Data), c('IDDD', 'Value'))))
    setcolorder(Datae1, ColNames)
    setcolorder(Datae2, ColNames)
    output.Data <- rbindlist(list(Datae1, Datae2))
    setkeyv(output.Data, names(output.Data)[-which(names(output.Data) == 'Value')])
    DupRows <- duplicated(output.Data, by = key(output.Data))
    if (sum(DupRows) > 0) {
      cat('[StQ::+] ATENTION!! Duplicated rows! If you are summing data sets
          corresponding to two different time periods, make sure that the time
          reference variable is included as unit qualifier (IDQual).\n\n
          The next rows are duplicated and will be removed:\n\n')
      print(output.Data[DupRows])
    }
    output.Data <- output.Data[!DupRows]

    # Generamos el objeto final
    output <- StQ(Data = output.Data, DD = outputDD)

    return(output)

}

