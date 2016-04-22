#' @title Return IDDD identifiers from an object
#'
#' @description \code{getIDDD} returns all IDDD identifiers from the input 
#' object.  
#' 
#' @param object Object whose IDDD identifiers are queried.
#'
#' @return In the case of \linkS4class{VarNameCorresp} it returns the IDDD
#' identifiers from all components of of its slot \code{VarNameCorresp}.
#'
#' @examples
#' # A more elaborate example
#' VarList <- list(MicroData = data.table(IDQual = c('NumIdEst','','', '', '', '', ''),
#'                 NonIDQual = c('', 'IsNatMarket', 'IsEuroMarket', 'IsRWMarket', '', '', ''),
#'                 IDDD = c('','','','','Turnover', 'Turnover', 'Turnover'),
#'                 NumIdEst = c('', '', '', '', '.', '.', '.'),
#'                 IsNatMarket = c('', '', '', '', '1', '0', '0'),
#'                 IsEuroMarket = c('', '', '', '', '', '1', '0'),
#'                 IsRWMarket = c('', '', '', '', '', '', '1'),
#'                 Unit1 = c('','','','','cn01', 'cn02', 'cn03')))
#' Example <- new(Class = 'VarNameCorresp', VarList)
#' getIDDD(Example)
#' 
#' @export
setGeneric("getIDDD", function(object){standardGeneric("getIDDD")})
#' @rdname getIDDD
#' 
#' @include VarNameCorresp-class.R
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("VarNameCorresp"),
    function(object){
        
        IDDD.list <- lapply(object, function(DT){
            
            out <- DT[['IDDD']]
            out <- out[out != '']
            return(out)
        })
        output <- unique(Reduce(c, IDDD.list, init = IDDD.list[[1]]))
        return(output)
    }
)
#' @rdname getIDDD
#' 
#' @include DD-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("DD"),
    function(object){
        
        DTNames <- setdiff(slotNames(object), 'VarNameCorresp')
        output <- c()
        for (sl in DTNames) {
            
            DT <- slot(object, sl)
            IDDD <- DT[Sort == 'IDDD', Variable]
            output <- c(output, IDDD)
        }
        
        output <- unique(output)
        return(output)
    }
)
#' @rdname getIDDD
#' 
#' @include StQ-class.R getData.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getIDDD",
    signature = c("StQ"),
    function(object){
        
        output <- unique(getData(object)[['IDDD']])
        return(output)
    }
)
