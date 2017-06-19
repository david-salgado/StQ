0#' @title Return slot \code{Data} from an object possibly subsetted to a set of variables
#'
#' @description \code{getData} returns slot \code{Data} from the input object possibly subsetted to
#' those variables specified as an input parameter. 
#'
#' Input objects can be of class:
#' \itemize{
#' \item \code{StQ}: The input parameters are an object of class \linkS4class{StQ} and a character
#' vector, \code{VarNames}, with variable names. It returns the \code{data.table} corresponding to
#' slot \code{Data} of such an object, but only with variables included in \code{VarNames}. If in 
#' \code{VarNames} only the roots of variable names in the column \code{IDDD} of slot \code{Data}
#' are specified, it returns a data set with all variables which have those roots.
#'
#' If no variable name is specified in \code{VarNames}, it returns the complete slot \code{Data}.
#'
#' \item \code{StQList}: The input parameters are an object of class \linkS4class{StQList} and a
#' character vector, \code{VarNames}, with variable names. It returns a list of \linkS4class{StQ}
#' objects, but only with variables included in \code{VarNames}, as it is explained above.
#'
#' \item \code{rawStQ}: The input parameters are an object of class \linkS4class{rawStQ} and a
#' character vector, \code{VarNames}, with variable names. It returns the \linkS4class{rawDatadt}
#' corresponding to slot \code{Data} of such an object, but only with variables included in
#' \code{VarNames}.
#'
#' \item \code{rawStQList}: The input parameters are an object of class \linkS4class{rawStQList} and
#'  a character vector, \code{VarNames}, with variable names. It returns a list of
#'  \linkS4class{rawStQ} objects, but only with variables included in \code{VarNames}.
#'
#' @param object Object whose (possibly subsetted) slot \code{Data} is queried.
#'
#' @param VarNames \code{Character} vector with the variable names subsetting the data set.
#'
#' @return In the case of \linkS4class{StQ}/\linkS4class{rawStQ} objects, it returns a
#' \linkS4class{data.table}/\linkS4class{rawDatadt} with key-value pair structure corresponding to
#' slot \code{Data} from the input object with the values of the variables restricted to variable
#' names specified in \code{VarNames}. In the case of \linkS4class{StQList}/\linkS4class{rawStQList}
#' objects, it returns a list of \linkS4class{StQ}/linkS4class{rawStQ} objects.
#'
#' @examples
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
#' # From an StQList object
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' QList <- vector('list', 12)
#' QList <- lapply(QList, function(x) ExampleStQ)
#' names(QList) <- TimePer
#' QList <- BuildStQList(QList)
#' VarNames <- c('Turnover', 'Employees_1.')
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

        QualVarNames <- VarNames[grep('_', VarNames)]
        IDDDVarNames <- setdiff(VarNames, QualVarNames)
        
        if (length(QualVarNames) != 0 && length(IDDDVarNames) != 0) {
            
            DD <- getDD(object)
            DT_aux <- VarNamesToDT(QualVarNames, DD)
            DT_QualVarNames <- merge(object$Data, DT_aux, by = intersect(names(object$Data), names(DT_aux)))
            DT_IDDDVarNames <- object$Data[IDDD %chin% IDDDVarNames]
            output <- merge(DT_QualVarNames, DT_IDDDVarNames, by = intersect(names(DT_QualVarNames), names(DT_IDDDVarNames)), all = TRUE)
            
        } else if(length(QualVarNames) != 0){
            
            DD <- getDD(object)
            DT_aux <- VarNamesToDT(QualVarNames, DD)
            output <- merge(object$Data, DT_aux, by = intersect(names(object$Data), names(DT_aux)))
            
        } else {
            
            output <- object$Data[IDDD %chin% IDDDVarNames]
            
        }
        
        setcolorder(output, names(object$Data))
        return(output)
      }
)

#' @rdname getData
#' @export
setMethod(
    f = "getData",
    signature = c("rawStQ"),
    function(object, VarNames){

        if (missing(VarNames)) return(object$rawData)
            
        return(object$rawData[IDDDKey %chin% ExtractNames(VarNames)])
    }
)

#' @rdname getData
#' @export
setMethod(
    f = "getData",
    signature = c("StQList"),
    function(object, VarNames){

        if (missing(VarNames)) return(object$Data)

        output <- lapply(object$Data, function(StQObj){
            
                    DD <- getDD(StQObj)
                    out <- getData(StQObj, VarNames)
                    out <- StQ(Data = out, DD = DD)
            
                })
        
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

        output <- lapply(object$Data, function(rawStQObj){rawStQObj[IDDDKey %in% ExtractNames(VarNames)]})
        return(output)

    }
)
