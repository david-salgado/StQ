#' @title Convert production unit names of an object of class \link{Variable} into their
#' corresponding statistical variable names (IDDD)
#'
#' @description \code{VariableIDDDNames} returns a \linkS4class{character} vector with the
#' statistical variable name (IDDD + Qualifiers) corresponding to the production unit variable name
#' of the \link{variable} object specified as input argument.
#'
#' @param Variable Object with its main attributes
#' 
#' @param Correspondence Object in which Variable will be included.
#' 
#' @return Returns a \code{character} vector with the corresponding IDDD variable name.
#'
#' @details IDDD and qualifiers compose together the so-called IDDDname of the variable by pasting
#' the IDDD identifier and each consecutive qualifier with an underscore _.
#'
#' @examples
#' data(ExampleStQ)
#' DD <- getDD(ExampleStQ)
#' Var <- new(Class = 'Variable', UnitName = 'LRTEmp',
#'                                IDDD = 'Employees',
#'                                QualsValues = list(ID = '', EmplType = ''),
#'                                Length = '8',
#'                                ClassVar = 'numeric',
#'                                ValueRegExp = '[.]+',
#'                                Formula = as.call(list('log( 1 + (Turnover / (Employees_1. + Employees_2.1.)))')),
#'                                SlotName = 'MicroData',
#'                                Literal = '')
#' IDDDName <- VariableIDDDNames(Var, DD)
#' 
#' @export
setGeneric("VariableIDDDNames", function(Variable, Correspondence){standardGeneric("VariableIDDDNames")})

#' @rdname VariableIDDDNames
#' 
#' @include getIDDD.R getIDQual.R
#'
#' @import data.table 
#' 
#' @export
setMethod(
    f = "VariableIDDDNames",
    signature = c("Variable", "DD"),
    function(Variable, Correspondence){
        
        IDDD <- Variable@IDDD
        QualsValues <- unlist(Variable@QualsValues)
        if (IDDD %in% getIDDD(Correspondence)) {
            
            DD <- Correspondence
            DD[['VNC']] <- NULL
            DD <- rbindlist(DD, fill = TRUE)
            DD <- DD[Variable == IDDD][1]
            Quals <- DD[, names(DD)[grep('Qual', names(DD))], with = FALSE]
            Quals <- as.vector(t(Quals))
            Quals <- Quals[Quals != '']
            if (!identical(Quals, names(QualsValues))) {
                
                stop(paste0('[VariableIDDDNames validation] The names of slot QualsValues in Var must be: ', paste0(Quals, collapse = ','), ' and in this order. If any  qualifier is not necessary, its value must be \'\' .'))
            }
        }
        
        IDQuals <- getIDQual(Correspondence)
        QualsValues <- QualsValues[!names(QualsValues) %in% IDQuals]
        IDDDName <- paste(IDDD, paste0(QualsValues, collapse = '_'), sep = '_')
        return(IDDDName)

}
)
