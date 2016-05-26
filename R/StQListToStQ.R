#' @title Method to transform an StQList object into a list of StQ's objects.
#'
#' @description \code{StQListToStQ} transform an StQList object into a list of
#' StQ's objects with a new variable "Period" to take in account the interval
#' times related to StQList object.
#'
#' This method creates a variable with the name \code{Period} in Data slots with
#' the period related to each Data and adds this variable to each DD slot of the
#' new StQ's objects.
#'
#' @param object Object of class \linkS4class{StQList} to be transformed.
#'
#' @return a list of objects of class \linkS4class{StQ}.
#' 
#' @export
setGeneric("StQListToStQ",
           function(object){standardGeneric("StQListToStQ")})

#' @rdname StQListToStQ
#'
#' 
#' @include StQList-class.R StQ-class.R getData.R getDD.R DatadtToDT.R BuildVNC.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "StQListToStQ",
    signature = c("StQList"),
    function(object){
        

        DD <- Reduce('+', getDD(object))
        
        VNCPer <- BuildVNC(list(MicroData = new(Class = 'VNCdt', 
                                                .Data = data.table(IDQual = c('Period'),
                                                                   NonIDQual = '',
                                                                   IDDD = '',
                                                                   Period = '.',
                                                                   Unit1 = '')))) 
        Microdt <- new( Class = 'DDdt',data.table(Variable = c('Period'),  
                                                  Sort = c('IDQual'),
                                                  Class = c('character'),
                                                  Qual1 = '',
                                                  ValueRegExp = '.+'))
        
        DDPer <- new(Class = 'DD', VarNameCorresp = VNCPer, ID = new(Class = 'DDdt'), MicroData = Microdt, ParaData = new(Class = 'DDdt'))
        
        DD <- DD + DDPer
        
        IDQual <- getIDQual(DD)
        NonIDQual <- getNonIDQual(DD)
        DatadtList <- lapply(getData(object), getData)
        DataList <- lapply(DatadtList, DatadtToDT)
        Periods <- names(DataList)

        for (Per in Periods) {
            set(DataList[[Per]], NULL, 'Period', Per)
            setcolorder(DataList[[Per]], c(intersect(IDQual, names(DataList[[Per]])), 
                                           intersect(NonIDQual, names(DataList[[Per]])),
                                           c('IDDD', 'Value')))
        }
        

        Datadt <- new(Class = 'Datadt', .Data = rbindlist(DataList))
        out <- new(Class = 'StQ', Data = Datadt, DD = DD)
        
        return(out)
    }
)
