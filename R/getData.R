<<<<<<< HEAD
0#' @title Return slot \code{Data} from an object possibly subsetted to a set of variables
=======
#' @title Return slot \code{Data} from an object possibly subsetted to a set of variables
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
<<<<<<< HEAD
#' 
#' \item \code{rawStQ}: The input parameters are an object of class \linkS4class{rawStQ} and a 
#' character vector, \code{VarNames}, with variable names. It returns the \linkS4class{rawDatadt} 
#' corresponding to slot \code{Data} of such an object, but only with variables included in 
#' \code{VarNames}.
#' 
=======
#' 
#' \item \code{rawStQ}: The input parameters are an object of class \linkS4class{rawStQ} and a 
#' character vector, \code{VarNames}, with variable names. It returns the \linkS4class{rawDatadt} 
#' corresponding to slot \code{Data} of such an object, but only with variables included in 
#' \code{VarNames}.
#' 
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
<<<<<<< HEAD
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
=======
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
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
#' @include DD.R VNC.R StQ.R rawStQ.R ExtractNames.R StQList.R rawStQList.R
#' 
#' @export
<<<<<<< HEAD
setGeneric("getData", function(object, VarNames){standardGeneric("getData")})

#' @rdname getData
#' 
#' @export
setMethod(
  f = "getData",
  signature = c("StQ"),
  function(object, VarNames){
    
    
    if (missing(VarNames)) return(copy(object[['Data']]))
    
    return(copy(object[['Data']])[IDDD %chin% VarNames])
=======
setGeneric("getData", function(object, VarNames, DDslot = 'MicroData'){standardGeneric("getData")})
#' @rdname getData
#' 
#' @include DD-class.R DatadtToDT.R
#' 
#' @export
setMethod(
  f = "getData",
  signature = c("DD"),
  function(object, VarNames, DDslot = 'MicroData'){
    
    if (length(DDslot) > 1){
          
        stop('[StQ::getData] The input parameter DDslot must be a character vector of length 1.\n')
    }
    
    if (!DDslot %in% slotNames(object)){
          
        stop('[StQ::getData] The input parameter DDslot is not a component of this DD object.\n')
    }
    output <- slot(object, DDslot)  
    if (missing(VarNames)) {
        
        return(output)
    
    } else {
        
        VarNames <- ExtractNames(VarNames)
        MissingVar <- setdiff(VarNames, output[['Variable']])
        if (length(MissingVar) > 0) stop(paste0('[StQ::getData] The variable(s) ', paste0(MissingVar, collapse = ', '), ' is/are not present in this DD object.\n'))
        output <- output[which(output[['Variable']] %in% VarNames)]
        return(output)
    } 
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
  }
)

#' @rdname getData
#' 
<<<<<<< HEAD
#' @export
setMethod(
    f = "getData",
    signature = c("rawStQ"),
    function(object, VarNames){
        
        if (missing(VarNames)) return(copy(object[['rawData']]))
        
        return(copy(object[['rawData']])[IDDDKey %chin% VarNames])
    }
)

#' @rdname getData
#' 
#' @export
setMethod(
    f = "getData",
    signature = c("StQList"),
    function(object, VarNames){
        
        if (missing(VarNames)) return(copy(object[['Data']]))
        
        output <- lapply(object$Data, function(StQObj){
            
            LocalOut <- getData(StQObj, VarNames)
            return(LocalOut)
            
        })
        return(output)
    }
=======
#' @include Datadt-class.R StQ-class.R getDD.R DDslotWith.R getNonIDQual.R getData.R ExtractNames.R VarNamesToDT.R getDD.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
  f = "getData",
  signature = c("StQ"),
  function(object, VarNames, DDslot = 'MicroData'){
    
    
    if (missing(VarNames)) return(copy(object@Data))

    if (length(DDslot) > 1){
          
        stop('[StQ::getData] The input parameter DDslot must be a character vector of length 1.\n')
    }
      
    DD <- getDD(object)
      
    if (!DDslot %in% slotNames(DD)){
          
        stop('[StQ::getData] The input parameter DDslot is not a component of the slot DD of this StQ object.\n')
    }

    for (VarName in VarNames){
 
        Varslot <- DDslotWith(DD, VarName, DDslot)
        Quals <- setdiff(names(Varslot), c('Variable', 'Sort', 'Class', 'Length', 'ValueRegExp'))
              
        NameQuals <- c()
        for (Qual in Quals){
            
            NameQuals <- c(NameQuals, DatadtToDT(Varslot)[Variable == ExtractNames(VarName)][[Qual]])
        }
         
        nonIDQuals <- getNonIDQual(Varslot)
     
        
        if (!all(nonIDQuals %in% NameQuals) & VarName != ExtractNames(VarName)){
            
            stop('[StQ::getData] Variable ', ExtractNames(VarName), ' has not any non-unit qualifiers, so VarName cannot be ', VarName, '.')
        }
    }

    VarNames.DT <- VarNamesToDT(VarNames, getDD(object))
    ColNames <- sort(names(VarNames.DT))
    for (col in ColNames){

        if (all(VarNames.DT[[col]] == '')) VarNames.DT[, (col) := NULL]
        
    }

    output <- merge(getData(object), VarNames.DT, by = names(VarNames.DT))

    if(dim(output)[1] == 0) {
      
      warning('[StQ::getData] No such variables in this data set.')
      return(output)
    
    }
    DataNames <- names(object@Data)
    setcolorder(output, DataNames)
    #Cols <- sort(names(output))
    #for (col in Cols){
    #  
    #  if (all(output[[col]] == '')) output[, (col) := NULL]
    #  
    #}

    NotPresent <- VarNames[which(!ExtractNames(VarNames) %in% unique(getData(object)[['IDDD']]))]
   
    if (length(NotPresent) > 0){
      
      warning(paste0('[StQ::getData] The following variables are not present in the data set: ', 
                     paste0(NotPresent, collapse = ', '),
                     '.\n They are not included in the output data.table.\n'))
      
    }
    
    output <- new(Class = 'Datadt', output)
    return(output)
  }
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
)

#' @rdname getData
#' 
#' @export
setMethod(
<<<<<<< HEAD
    f = "getData",
    signature = c("rawStQList"),
    function(object, VarNames){
        
        if (missing(VarNames)) return(copy(object[['Data']]))
        
        output <- lapply(object$Data, function(rawStQObj){
            
            LocalOut <- getData(rawStQObj, VarNames)
            return(LocalOut)
            
        })
        return(output)
    }
=======
  f = "getData",
  signature = c("StQList"),
  function(object, VarNames, DDslot = 'MicroData'){
    
      DataList <- object@Data
      DDList <- lapply(DataList, function(x){x@DD})
      if (missing(VarNames)){

        DataList <- lapply(DataList, getData, DDslot = DDslot)
        
      } else {

        DataList <- lapply(DataList, function(x){getData(x, VarNames = VarNames, DDslot = DDslot)})
      }
      
      output <- lapply(seq(along = DataList), function(index){new(Class = 'StQ', Data = DataList[[index]], DD = DDList[[index]])})
      
      names(output) <- getPeriods(object)
      
      return(output)
  }
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
)
#' @rdname getData
#' 
#' @include rawStQ-class.R rawDatadt-class.R ExtractNames.R DatadtToDT.R 
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getData",
    signature = c("rawStQ"),
    function(object, VarNames, DDslot = 'MicroData'){
        
        
        if (missing(VarNames)) return(copy(object@Data))
        
        rootVarNames <- ExtractNames(VarNames)
        
        output <- DatadtToDT(object@Data)[IDDDKey %in% rootVarNames]
        return(output)

    }
)
#' @rdname getData
#' 
#' @include rawStQList-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getData",
    signature = c("rawStQList"),
    function(object, VarNames, DDslot = 'MicroData'){
        
        DataList <- object@Data
        DDList <- lapply(DataList, function(x){x@DD})
        if (missing(VarNames)){
            
            DataList <- lapply(DataList, getData, DDslot = DDslot)
            
        } else {
            
            DataList <- lapply(DataList, function(x){
                
                DT <- getData(x, VarNames = VarNames, DDslot = DDslot)
                out <- new(Class = 'rawDatadt', DT)
                return(out)
            })
        }

        output <- lapply(seq(along = DataList), function(index){new(Class = 'rawStQ', Data = DataList[[index]], DD = DDList[[index]])})
        
        names(output) <- getPeriods(object)
        
        return(output)
    }
)
