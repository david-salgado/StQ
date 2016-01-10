#' @title Method \code{+} for the class \linkS4class{DD}
#'
#' @description \code{+} sums two objects of class \linkS4class{DD}. This method
#' overloads the operator \link{+} and returns a new object of class
#' \linkS4class{DD}.
#'
#' @param e1 Object of class \linkS4class{DD}.
#'
#' @param e2 Object of class \linkS4class{DD}.
#'
#' @return Object of class \linkS4class{DD} resulting from integrating both
#' \linkS4class{DD} objects in a single \linkS4class{DD} object.
#'
#' @examples
#' library(data.table)
#' DD1 <- new(Class = 'DD',
#'            Data = data.table(Variable = c('NOrden', 'CCAA', 'CifraNeg'),
#'                              Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                              Class = c('character', 'character', 'numeric'),
#'                              Qual1 = c('', '', 'NOrden')))
#' DD2 <- new(Class = 'DD',
#'            Data = data.table(Variable = c('logCifraNeg'),
#'                              Sort = c('IDDD'),
#'                              Class = c('numeric'),
#'                              Qual1 = c('NOrden')))
#' DD1 + DD2
#'
#' @include DD-class.R getData.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "+",
    signature = c("DD", "DD"),
    definition = function(e1, e2){

        Data1 <- getData(e1)
        colNames1 <- names(Data1)

        Data2 <- getData(e2)
        colNames2 <- names(Data2)

        CommonCol <- intersect(colNames1, colNames2)
        col1Not2 <- setdiff(colNames1, colNames2)
        if (length(col1Not2) > 0) {

            for (NewCol in col1Not2){

                Data2[, NewCol := '', with = F]

            }
        }
        col2Not1 <- setdiff(colNames2, colNames1)
        if (length(col2Not1) > 0) {

            for (NewCol in col2Not1){

                Data1[, NewCol := '', with = F]

            }
        }

        output <- rbindlist(list(Data1, Data2))
        setkeyv(output, names(output))
        output <- output[!duplicated(output)]
        output <- new(Class = 'DD', Data = output)
        return(output)

    }
)
