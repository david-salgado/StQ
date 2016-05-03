#' @title Return slot Data from an object
#'
#' @description \code{getData} returns slot \code{Data} from the input object.  
#' 
#' In the case of objects of class \linkS4class{DD}, it returns the slot
#' \code{MicroData} of the input object.
#' 
#' In the case of objects of class \linkS4class{StQ}, it returns a data set 
#' restricted to those root variable names in the column \code{IDDD} of slot
#' \code{Data} specified in the input parameter \code{VarNames}.
#' 
#' Input objects can be of class: 
#' \itemize{
#' \item \code{StQ}: The input parameters are an object of class 
#' \linkS4class{StQ} and a character vector, \code{VarNames}, with variable 
#' names. It returns the \code{data.table} corresponding to slot \code{Data} of 
#' such an object, but only with variables included in \code{VarNames}.
#'
#' If no variable name is specified in \code{VarNames}, it returns the complete  
#' slot \code{Data}. 
#' 
#' \item \code{DD}: The input parameter is an object of class \linkS4class{DD}. 
#' The parameter \code{VarNames} has no effect. 
#' }
#' 
#' @param object Object whose (possibly subsetted) slot \code{Data} is queried.
#'
#' @param VarNames Character vector with the variable names subsetting the data 
#' set. 
#'
#' @param DDslot Character vector of length 1 with the name of DD slot in which
#' variables in VarNames are defined. Its default value is \code{MicroData} and
#' has no efect if the input object is a \linkS4class{DD} object.
#' 
#' @return In the case of \linkS4class{StQ} objects, it returns a 
#' \linkS4class{data.table} with key-value pair structure corresponding to slot
#' \code{Data} from the input object with the values of the column \code{IDDD} 
#' restriCted to variable names specified in \code{VarNames}. 
#'
#' @examples
#' # From DD objects 
#' data(ExampleDD)
#' getData(ExampleDD)
#' 
#' # From an StQ object 
#' VarNames <- c('Orders_0')
#' getData(ExampleQ, VarNames)
#' 
#' VarNames <- c('Turnover')
#' getData(ExampleQ, VarNames)
#'
#' # From an StQList object 
#' mm <- c(paste0('0', 1:9), 10:12)
#' TimePer <- paste0('MM', mm, '2015')
#' QList <- vector('list', 12)
#' QList <- lapply(QList, function(x) ExampleQ)
#' names(QList) <- TimePer
#' QList <- new(Class = 'StQList', QList)
#' VarNames <- c('IASSCifraNeg', 'IASSEmpleo')
#' getData(QList, VarNames)
#' 
#' @export
setGeneric("getData", function(object, VarNames, DDslot = 'MicroData'){standardGeneric("getData")})
#' @rdname getData
#' 
#' @include DD-class.R
#' 
#' @export
setMethod(
  f = "getData",
  signature = c("DD"),
  function(object, VarNames, DDslot = 'MicroData'){
    
    out <- copy(object@MicroData)
    return(out)
  }
)
#' @rdname getData
#' 
#' @include Datadt-class.R StQ-class.R getDD.R getSlotDD.R getNonIDQual.R getData.R ExtractNames.R VarNamesToDT.R getDD.R
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
          
        stop('[StQ::getData] DDslot must be a character vector of length 1.')
    }
      
    DD <- getDD(object)
      
    if (!DDslot %in% slotNames(DD)){
          
        stop('[StQ::getData] DDslot is not a component of the slot DD of the input object.')
    }
 
    for (VarName in VarNames){
 
        Varslot <- getSlotDD(DD, VarName, DDslot)
        Quals <- setdiff(names(Varslot),
                         c('Variable', 'Sort', 'Class', 'ValueRegExp'))
        
        NameQuals <- c()
        for (Qual in Quals){
            
            NameQuals <- c(NameQuals,Varslot[Variable == ExtractNames(VarName)][[Qual]])
        }
        
        nonIDQuals <- getNonIDQual(Varslot)
        
        
        if (!all(NameQuals %in% nonIDQuals) & VarName != ExtractNames(VarName)){
            
            stop('[StQ::getData] Variable ', ExtractNames(VarName), ' has not any non-identity qualifiers, so VarName cannot be ', VarName, '.')
        }
    }
    

    VarNames.DT <- VarNamesToDT(VarNames, getDD(object))
    for (col in names(VarNames.DT)){
        
        if (all(VarNames.DT[[col]] == '')) VarNames.DT[, col := NULL, with = F]
        
    }
    setkeyv(VarNames.DT, names(VarNames.DT))
    DataNames <- names(object@Data)
    setkeyv(getData(object), names(VarNames.DT))
    output <- merge(getData(object), VarNames.DT)

    if(dim(output)[1] == 0) {
      
      warning('[StQ::getData] No such variables in this data set.')
      return(output)
    }
    setcolorder(output, DataNames)
    Cols <- sort(names(output))
    for (col in Cols){
      
      if (all(output[[col]] == '')) output[, col := NULL, with = F]
      
    }

    NotPresent <- VarNames[which(!ExtractNames(VarNames) %in% unique(getData(object)[['IDDD']]))]
   
    if (length(NotPresent) > 0){
      
      warning(paste0('[StQ::getData] The following variables are not present in the data set: ', 
                     paste0(NotPresent, collapse = ', '),
                     '.\n They are not included in the output data.table.'))
      
    }
    
    output <- new(Class = 'Datadt', output)
    return(output)
  }
)
#' @rdname getData
#' 
#' @include StQList-class.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
  f = "getData",
  signature = c("StQList"),
  function(object, VarNames, DDslot = 'MicroData'){
    
    if (missing(VarNames)){
      
      output <- lapply(object@Data, function(x) getData(x))
      
      return(output)
    }
    
    output <- lapply(object@Data, function(x) getData(x, VarNames))  
    
    return(output)
  }
)
#' @rdname getData
#' 
#' @include rawStQ-class.R rawDatadt-class.R getDD.R getSlotDD.R getNonIDQual.R ExtractNames.R 
#' 
#' @import data.table
#' 
#' @export
setMethod(
    f = "getData",
    signature = c("rawStQ"),
    function(object, VarNames, DDslot = 'MicroData'){
        
        
        if (missing(VarNames)) return(copy(object@Data))
        
        DD <- getDD(object)
        
        for (VarName in VarNames){
            
            Varslot <- getSlotDD(DD, VarName, DDslot)
        }
        
        Quals <- setdiff(names(Varslot),
                         c('Variable', 'Sort', 'Class', 'ValueRegExp'))
        for (VarName in VarNames){
            
            NameQuals <- c()
            for (Qual in Quals){
                
                NameQuals <- c(NameQuals,Varslot[Variable == ExtractNames(VarName)][[Qual]])
            }
            
            nonIDQuals <- getNonIDQual(Varslot)
            
            
            if (!all(NameQuals %in% nonIDQuals) & VarName != ExtractNames(VarName)){
                
                stop('[rawStQ::getData] Variable ', ExtractNames(VarName), ' has not any non-identity qualifiers, so VarName cannot be ', VarName, '.')
            }
            
        }
        
        key <- object@Data[['Key']]
        VarNamesKey <- paste0('IDDD:', VarNames)
        listVarNames.DT <- lapply(VarNamesKey, function(x){grep(x,key@.Data)})
        VarNames.DT <- unlist(listVarNames.DT)
        NotPresent <- c()
        for(i in seq(along = listVarNames.DT)){
            
            if(length(listVarNames.DT[[i]]) == 0){
                
                NotPresent <- c(NotPresent, VarNames[i])
            }
        }
            
        if(length(VarNames.DT) == 0){
            
            stop('[rawStQ::getData] No such variables in this data set.')
        }else if(length(NotPresent) > 0){
            
            warning(paste0('[rawStQ::getData] The following variables are not present in the data set: ', 
                           paste0(NotPresent, collapse = ', '),
                           '.\n They are not included in the output data.table.'))
        }
        
        key <- object@Data[['Key']][VarNames.DT]
        key <- new(Class = 'rawKey', key)
        value <- object@Data[['Value']][VarNames.DT]
        output <- new(Class = 'rawDatadt', data.table(Key = key, Value = value))
        
        
        return(output)
    }
)
