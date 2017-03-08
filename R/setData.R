#' @title Set value of slot \code{Data}
#'
#' @description \code{setData} assigns a \linkS4class{Datadt} to the slot \code{Data} of the input
#' object.
#'
#' @param object Object whose slot \code{Data} is to be assigned.
#'
#' @param value \linkS4class{Datadt} to be assigned to the slot \code{Data}.
#'
#' @return Object with slot Data updated.
#'
#' @examples
#' # We build an empty data.table:
#' library(data.table)
#' Data <- new(Class = 'Datadt', data.table(IDDD = character(0), Value = numeric(0)))
#'
#' # We assign this data.table to the slot Data of object NewExampleStQ:
#' data(ExampleStQ)
#' NewExampleStQ <- ExampleStQ
#' setData(NewExampleStQ) <- Data
#' NewExampleStQ
#'
#' @rdname setData
#' 
#' @include StQ.R 
#'
#' @import data.table
#'
#' @export
setGeneric("setData<-", function(object, value){standardGeneric("setData<-")})

#' @rdname setData
#'
<<<<<<< HEAD
=======
#' @include StQ-class.R 
#'
#' @import data.table
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @export
setReplaceMethod(
    f = "setData",
    signature = c("StQ", "Datadt"),
    function(object, value){
        
<<<<<<< HEAD
        object$Data <- value
        return(object)
    }
)

#' @rdname setData
#'
#' @export
setReplaceMethod(
    f = "setData",
    signature = c("rawStQ", "data.table"),
    function(object, value){
        
        object$Data <- value
=======
        value <- DatadtToDT(value)
        setkeyv(value, setdiff(names(value), 'Value'))
        object@Data <- new(Class = 'Datadt', value)
        validObject(object)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        return(object)
    }
)
