#' @title Return class for each input variable from the dictionary of the input object
#'
#' @description \code{getClass} returns the class for each input variable from the dictionary of the 
#' input object.
#'
#' @param object Object whose class for each input variable in the slot \linkS4class{DD} are 
#' required.
#'
#' @param variables \code{Character} vector with the names of the variables whose class is required.
#'
#' @return Returns a character vector with the class of each input variables. If \code{variables} is 
#' \code{NULL} all variables in the dictionary are considered.
#'
#' @examples
#' getClass(ExampleStQ)
#'
#'
#' @export
setGeneric("getClass", function(object, variables = NULL){standardGeneric("getClass")})

#' @rdname getClass
#'
#' @include VNC.R DD.R StQ.R getVariables.R getDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getClass",
    signature = c("DD"),
    function(object, variables = NULL){
        
        allVariables <- getVariables(object)
        invalidVariables <- variables[!variables %in% allVariables]
        if (length(invalidVariables) != 0) {
            
            stop(paste0('[StQ::getClass] The following variables are not present in object: ',
                        paste0(invalidVariables, collapse = ', '),
                        '.\n'))
        }
        DDslots <- setdiff(names(object), 'VNC')
        DDslots.list <- lapply(DDslots, function(x) object[[x]])
        completeDDdt <- rbindlist(DDslots.list, fill = TRUE)
        completeDDdt <- completeDDdt[!duplicated(completeDDdt, by = c('Variable', 'Class'))]
        classes <- completeDDdt[['Class']]
        names(classes) <- completeDDdt[['Variable']]
        if (!is.null(variables)) classes <- classes[variables]
        return(classes)
    }
)

#' @rdname getClass
#'
#' @export
setMethod(
    f = "getClass",
    signature = c("StQ"),
    function(object, variables = NULL){
        
        output <- getClass(getDD(object), variables = variables)
        return(output)
    }
)
