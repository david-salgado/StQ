#' @title Method \code{+} for the class \linkS4class{Datadt}
#'
#' @description \code{+} joins two objects of class \linkS4class{Datadt} in a
#' single object of the same class.
#'
#' This method overloads the operator \code{\link{+}} and builds a new
#' \linkS4class{Datadt} object joining both input objects.
#'
#' @param e1 Object of class \code{Datadt}.
#'
#' @param e2 Object of class \code{Datadt}.
#'
#' @return Object of class \code{Datadt}.
#'
#' @examples
#' # We build two trivial data sets:
#' library(data.table)
#' Ddt1 <- new(Class = 'Datadt', 
#'         data.table(ID = c('001', '001', '001', '001'), 
#'                    IsNatMarket = c('0', '1', '', ''),
#'                    IDDD = c('Turnover', 'Turnover', 'Province', 'NACE09'),
#'                    Value = c('625000', '23154', '04', '0512')))
#'
#' Ddt2 <- new(Class = 'Datadt', 
#'         data.table(ID = c('001', '002', '002'), 
#'                    IDDD = c( 'Turnover', 'CCAA', 'IASSCifraNeg'),
#'                    Value = c('648154', '25', '45645')))
#'
#' Ddt <- Ddt1 + Ddt2
#' show(Ddt)
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "+",
    signature = c("Datadt", "Datadt"),
    definition = function(e1, e2){
        
        #Incluimos las mismas columnas en ambos objetos
        e1 <- DatadtToDT(e1)
        e2 <- DatadtToDT(e2)
        ColNames.e1 <- copy(names(e1))
        ColNames.e2 <- copy(names(e2))
        NewCol.e2 <- setdiff(ColNames.e2, ColNames.e1)
        NewCol.e1 <- setdiff(ColNames.e1, ColNames.e2)
        
        if (length(NewCol.e2) > 0) e1 <- copy(e1)[, NewCol.e2 := character(.N), with = FALSE]
        if (length(NewCol.e1) > 0) e2 <- copy(e2)[, NewCol.e1 := character(.N), with = FALSE]
        
        # Unimos los slots Data con rbindlist eliminando los duplicados
        ColNames <- c(setdiff(names(e1), c('IDDD', 'Value')), setdiff(names(e2), setdiff(names(e1), c('IDDD', 'Value'))))
        setcolorder(e1, ColNames)
        setcolorder(e2, ColNames)
        output.Data <- rbindlist(list(e1, e2))
        setkeyv(output.Data, names(output.Data)[-which(names(output.Data) == 'Value')])
        DupRows <- duplicated(output.Data)
        if (sum(DupRows) > 0) {
            cat('[Datadt::+] ATENTION!! Duplicated rows! If you are summing data sets
                corresponding to two different time periods, make sure that the time
                reference variable is included as unit qualifier (IDQual).\n\n
                The next rows are duplicated and will be removed:\n\n')
            print(output.Data[DupRows])
        }
        output.Data <- output.Data[!DupRows]
        
        # Generamos el objeto final
        output <- new(Class = 'Datadt', output.Data)
        
        validObject(output)
        
        return(output)
        
    }
            )
