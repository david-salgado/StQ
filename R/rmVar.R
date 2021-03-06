#' Remove variables from an object
#'
#' \code{rmVar} removes from the input object those values for variable names in the input parameter
#' \code{VarNames}.
#'
#' This method returns the same input object with those variables specified in the input parameter
#' \code{VarNames} removed from the data set.
#'
#' @param object Input object of class \link{StQ} to remove the variables from.
#'
#' @param VarNames Character vector with the variable names to remove.
#'
#' @return Object with the same class as the input object but where the variables in \code{VarNames}
#' have been removed. Variables are removed only from slot \code{Data}.
#' 
#' @include StQ.R ExtractNames.R getData.R setData.R
#'
#' @examples
#' data(ExampleStQ)
#' rmVar(ExampleStQ)
#' rmVar(ExampleStQ, 'TotalEmpl')
#'
#' @export
setGeneric("rmVar",
           function(object, VarNames = character(0)) {standardGeneric("rmVar")})
#' @rdname rmVar
#'
#' @import data.table
#'
#'
#' @export
setMethod(
    f = "rmVar",
    signature = c("StQ"),
    function(object, VarNames = character(0)){

        ## Se eliminan las variables especificadas de los slots Data, pero no de DD.

        if (length(VarNames) == 0) {
          cat('[StQ::rmVar] No variable specified. The input object is returned.\n')
          return(object)
        }

        Data.VarNames <- unique(getData(object)[['IDDD']])
        NotPresentVar <- setdiff(ExtractNames(VarNames), Data.VarNames)
        if (length(NotPresentVar) != 0) {
            cat('[StQ::rmVar] The following variables are not present in the slot Data:\n')
            print(paste0(paste0(NotPresentVar, collapse = ', '), '.\n'))
        }

        PresentVar <- intersect(ExtractNames(VarNames), Data.VarNames)

        if (length(PresentVar) == 0) {
            cat('[StQ::rmVar] No specified variables in slot Data. No variable will be removed.\n')
            return(object)

        } else {

            setData(object) <- getData(object)[!IDDD %in% PresentVar]

            return(object)
        }
    }
)
