#' @title Set value of slot \code{Data}
#'
#' @description \code{setData} assigns a \linkS4class{data.table} to the slot \code{Data} of the 
#' input object.
#'
#' @param object Object whose slot \code{Data} is to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{Data}.
#'
#' @return Object with slot Data updated.
#'
#' @examples
#' # We build an empty data.table:
#' library(data.table)
#' Data <- data.table(IDDD = character(0), Value = numeric(0))
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
#' @export
setReplaceMethod(
    f = "setData",
    signature = c("StQ", "data.table"),
    function(object, value){
        
      DD <- getDD(object)
      IDDD <- unique(value[['IDDD']])
      Quals <- c(getIDQual(object), getNonIDQual(object))
      
      for (var in IDDD) {
        
        for (Sheet in setdiff(names(DD), 'VNC')) {
          
          DDdt <- DD[[Sheet]]
          if (var %in% DD[[Sheet]][['Variable']]) {
            
            Quals.var <- t(DDdt[Variable == var, names(DDdt)[grep('Qual', names(DDdt))], with = FALSE])[, 1]
            Quals.var <- Quals.var[Quals.var != '']
            Quals <- unique(c(Quals, Quals.var))
            break
          }
        }
      }
      
      value <- value[, c(intersect(names(value), Quals), 'IDDD', 'Value'), with = FALSE]
      
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
        
        object$rawData <- value
        return(object)
    }
)
