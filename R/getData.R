0#' @title Return slot \code{Data} from an object possibly subsetted to a set of variables
#'
#' @description \code{getData} returns slot \code{Data} from the input object possibly subsetted to
#' those variables specified as an input parameter. In the case of those variables pertaining to
#' more than one slot of the input object (in particular for \linkS4class{DD} objects), a third
#' parameter \code{DDslot} with default value MicroData must be specified.
#'
#' In the case of objects of class \linkS4class{DD}, it returns the slot \code{MicroData} of the
#' input object.
#'
#' In the case of objects of class \linkS4class{StQ}, it returns a data set restricted to those root
#' variable names in the column \code{IDDD} of slot \code{Data} specified in the input parameter
#' \code{VarNames}.
#'
#' Input objects can be of class:
#' \itemize{
#' \item \code{StQ}: The input parameters are an object of class \linkS4class{StQ} and a character
#' vector, \code{VarNames}, with variable names. It returns the \code{data.table} corresponding to
#' slot \code{Data} of such an object, but only with variables included in \code{VarNames}.
#'
#' If no variable name is specified in \code{VarNames}, it returns the complete slot \code{Data}.
#'
#' \item \code{DD}: The input parameter is an object of class \linkS4class{DD}. The parameter
#' \code{VarNames} has no effect.
#'
#' \item \code{StQList}: The input parameters are an object of class \linkS4class{StQList} and a
#' character vector, \code{VarNames}, with variable names. It returns a list of \linkS4class{StQ}
#' objects, but only with variables included in \code{VarNames}.
#'
#' \item \code{rawStQ}: The input parameters are an object of class \linkS4class{rawStQ} and a
#' character vector, \code{VarNames}, with variable names. It returns the \linkS4class{rawDatadt}
#' corresponding to slot \code{Data} of such an object, but only with variables included in
#' \code{VarNames}.
#'
#' \item \code{rawStQList}: The input parameters are an object of class \linkS4class{rawStQList} and
#'  a character vector, \code{VarNames}, with variable names. It returns a list of
#'  \linkS4class{rawStQ} objects, but only with variables included in \code{VarNames}.
#
#' }
#'
#' @param object Object whose (possibly subsetted) slot \code{Data} is queried.
#'
#' @param VarNames \code{Character} vector with the variable names subsetting the data set.
#'
#' @param DDslot \code{Character} vector of length 1 with the name of DD slot whose variables are
#' queried in the input parameter VarNames. Its default value is \code{MicroData} and has no efect
#' if the input object is a \linkS4class{DD} object.
#'
#' @return In the case of \linkS4class{StQ}/\linkS4class{rawStQ} objects, it returns a
#' \linkS4class{data.table}/\linkS4class{rawDatadt} with key-value pair structure corresponding to
#' slot \code{Data} from the input object with the values of the column \code{IDDD} restriCted to
#' variable names specified in \code{VarNames}. In the case of \linkS4class{DD} objects, it returns
#' the slot MicroData. In the case of \linkS4class{StQList}/\linkS4class{rawStQList} objects, it
#' returns a list of \linkS4class{StQ}/linkS4class{rawStQ} objects.
#'
#' @examples
#' # From DD objects
#' data(ExampleDD)
#' getData(ExampleDD)
#' getData(ExampleDD, DDslot = 'Aggregates')
#' getData(ExampleDD, VarNames = c('Turnover', 'Employees_1.'))
#'
#' # From an StQ object
#' VarNames <- c('Employees_1.')
#' getData(ExampleStQ, VarNames)
#'
#' VarNames <- c('Turnover')
#' getData(ExampleStQ, VarNames)
#'
#' # From a rawStQ object
#' VarNames <- c('Turnover')
#' getData(ExamplerawStQ, VarNames)
#'
#' VarNames <- c('Turnover')
#' getData(ExampleStQ, VarNames)
#'
#' # From an StQList object
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' QList <- vector('list', 12)
#' QList <- lapply(QList, function(x) ExampleStQ)
#' names(QList) <- TimePer
#' QList <- BuildStQList(QList)
#' VarNames <- c('Turnover', 'Employees_2.1')
#' getData(QList, VarNames)
#'
#' @include StQ.R rawStQ.R  StQList.R rawStQList.R
#'
#' @export
setGeneric("getData", function(object, VarNames){standardGeneric("getData")})

#' @rdname getData
#'
#' @export
setMethod(
    f = "getData",
    signature = c("StQ"),
    function(object, VarNames){

        if (missing(VarNames)) return(object$Data)

        return(object$Data[IDDD %chin% VarNames])
      }
)

#' @rdname getData
#' @export
setMethod(
    f = "getData",
    signature = c("rawStQ"),
    function(object, VarNames){

        if (missing(VarNames)) return(object$rawData)

        return(object$rawData[IDDDKey %chin% VarNames])
    }
)

#' @rdname getData
#' @export
setMethod(
    f = "getData",
    signature = c("StQList"),
    function(object, VarNames){

        if (missing(VarNames)) return(object$Data)

        output <- lapply(object$Data, function(StQObj){StQObj[IDDD %in% VarNames]})
        return(output)

    }
)

#' @rdname getData
#' @export
setMethod(
    f = "getData",
    signature = c("rawStQList"),
    function(object, VarNames){

        if (missing(VarNames)) return(object$Data)

        output <- lapply(object$Data, function(rawStQObj){rawStQObj[IDDDKey %in% VarNames]})
        return(output)

    }
)
