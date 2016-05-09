#' @title Extract edit names from an object
#'
#' @description This method identifies the IDQual qualifiers in the input object
#' and returns a \linkS4class{data.table} with the values of these qualifiers
#' for each statistical unit and their corresponding edit names.
#'
#' @param object Object of class \linkS4class{StQ}.
#'
#' @return It returns a \code{data.table} with the statistical units in the
#' input object with their corresponding edit names.
#'
#' @examples
#' \dontrun{
#' data(ExampleStQ)
#' getEditNames(ExampleStQ)
#' }
#' 
#' @export
setGeneric("getEditNames", function(object) {standardGeneric("getEditNames")})

#' @rdname getEditNames
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getEditNames",
    signature = c("data.table"),
    function(object){
        
        # For files FL; these files should be reformatted in the repo
        # In Spanish
        if (all(c('LimInf', 'LimSup') %in% names(object))){
            
            IDQual <- setdiff(names(object), 
                              c('Mes', 'NomControl', 'Condicion', 'LimInf', 'LimSup'))
            Edits <- object[, c(IDQual, 'NomControl'), with = FALSE]
            setkeyv(Edits, IDQual)
            return(Edits)
            
        }
        # In English    
        if (all(c('LowBound', 'UpperBound') %in% names(object))){
            
            IDQual <- setdiff(names(object), 
                              c('Month', 'EditName', 'Condition', 'LowBound', 'UpperBound'))
            Edits <- object[, c(IDQual, 'EditName'), with = FALSE]
            setkeyv(Edits, IDQual)
            return(Edits)
        }
        
        # For files FT; these files should be reformatted in the repo
        # In Spanish
        if (all(paste0(c('literal', 'error', 'std', 'quant'), '1') %in% names(object))){
            
            NeditsIDQual <- dim(object)[2] - 2
            Nedits <- (NeditsIDQual - NeditsIDQual %% 4) / 4
            IDQual <- setdiff(names(object), c(paste0(c('literal'), 1:Nedits),
                                               paste0(c('error'), 1:Nedits),
                                               paste0(c('std'), 1:Nedits),
                                               paste0(c('quant'), 1:Nedits),
                                               'Quant_PesoDis', 'Quant_FMoments'))
            Edits <- object[, c(IDQual, paste0('literal', 1:Nedits)), with = FALSE]
            setkeyv(Edits, IDQual)
            return(Edits)
        }
        
    }
)
