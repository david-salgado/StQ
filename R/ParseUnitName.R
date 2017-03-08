#' @title Produce a vector of file names parsed out of a pattern and a sequence of time period names
#' 
#' @description This function parses each component of a vector of file names with a pattern filling
#'  out the corresponding codes with the input time periods.
#' 
#' @param UnitNames Character vector with the names of the files to parse.
#' 
#' @param MetaValues Object of the appropriate class (\linkS4class{RepoTimeInt}, 
#' \linkS4class{integer}, ...) with the values of the metacharacters to be parsed.
#' 
#' @return Return a vector with each component of the input vector \code{UnitNames} parsed according
#'  to the input parameter \code{TimePeriods}.
#' 
#' @examples
#' library(RepoTime)
#' ParseUnitName(c('dat[mm][aa].sas7bdat'), newRepoTime(c('MM122015', 'MM012016', 'MM022016')))
#' ParseUnitName(c('dat[mm][aa].sas7bdat', 'in[aaaa][mm].txt'), 
#'               newRepoTime(c('MM122015', 'MM012016', 'MM022016')))
#' ParseUnitName(c('PONALI_[ccaa]'), c(paste0(0, 1:9), 10:12))
#'       
#' @export
setGeneric("ParseUnitName", function(UnitNames, MetaValues){standardGeneric("ParseUnitName")})

#' @rdname ParseUnitName
#' 
#' @import data.table RepoTime
#' 
#' @export
setMethod(
    f = "ParseUnitName",
    signature = c("character", "RepoTimeInt"),
    function(UnitNames, MetaValues){
    
    TimePeriods <- getRepo(MetaValues)
    PeriodType <- unique(substr(TimePeriods, 1, 1))
    if (length(PeriodType) != 1) stop('[RepoReadWrite::ParseUnitName] Only one time period type allowed at a time.')
    
    if (PeriodType == 'M') {
        
        output <- unlist(lapply(TimePeriods, function(TimePeriod){
            
            month <- unlist(substr(TimePeriod, 3, 4))
            ParsedUnitNames <- unlist(lapply(UnitNames, function(UnitName){
                
                outLocal <- gsub('[mm]', month, UnitName, fixed = TRUE)
                return(outLocal)
            }))
            year4 <- unlist(as.integer(substr(TimePeriod, 5, 8)))
            ParsedUnitNames <- unlist(lapply(ParsedUnitNames, function(UnitName){
                
                outLocal <- gsub('[aaaa + 1]', year4 + 1, UnitName, fixed = TRUE)
                outLocal <- gsub('[aaaa]', year4, outLocal, fixed = TRUE)
                outLocal <- gsub('[aaaa - 1]', year4 - 1, outLocal, fixed = TRUE)
                return(outLocal)
            }))
            year2 <- unlist(as.integer(substr(TimePeriod, 7, 8)))
            ParsedUnitNames <- unlist(lapply(ParsedUnitNames, function(UnitName){
                
                outLocal <- gsub('[aa + 1]', year2 + 1, UnitName, fixed = TRUE)
                outLocal <- gsub('[aa]', year2, outLocal, fixed = TRUE)
                outLocal <- gsub('[aa - 1]', year2 - 1, outLocal, fixed = TRUE)
                return(outLocal)
            }))
        }))
    }
    if (PeriodType == 'A') {
        
        output <- unlist(lapply(TimePeriods, function(TimePeriod){
            
            year4 <- unlist(as.integer(substr(TimePeriod, 3, 6)))
            ParsedUnitNames <- unlist(lapply(UnitNames, function(UnitName){
                
                outLocal <- gsub('[aaaa + 1]', year4 + 1, UnitName, fixed = TRUE)
                outLocal <- gsub('[aaaa]', year4, outLocal, fixed = TRUE)
                outLocal <- gsub('[aaaa - 1]', year4 - 1, outLocal, fixed = TRUE)
                return(outLocal)
            }))
            year2 <- unlist(as.integer(substr(TimePeriod, 5, 6)))
            ParsedUnitNames <- unlist(lapply(ParsedUnitNames, function(UnitName){
                
                outLocal <- gsub('[aa + 1]', year2 + 1, UnitName, fixed = TRUE)
                outLocal <- gsub('[aa]', year2, outLocal, fixed = TRUE)
                outLocal <- gsub('[aa - 1]', year2 - 1, outLocal, fixed = TRUE)
                return(outLocal)
            }))
        }))
    }
    
    if (PeriodType == 'T') {
        
        output <- unlist(lapply(TimePeriods, function(TimePeriod){
            
            
            term <- unlist(substr(TimePeriod, 3, 3))
            ParsedUnitNames <- unlist(lapply(UnitNames, function(UnitName){
                
                outLocal <- gsub('[t]', term, UnitName, fixed = TRUE)
                return(outLocal)
            }))
            year4 <- unlist(as.integer(substr(TimePeriod, 4, 7)))
            ParsedUnitNames <- unlist(lapply(ParsedUnitNames, function(UnitName){
                
                outLocal <- gsub('[aaaa + 1]', year4 + 1, UnitName, fixed = TRUE)
                outLocal <- gsub('[aaaa]', year4, outLocal, fixed = TRUE)
                outLocal <- gsub('[aaaa - 1]', year4 - 1, outLocal, fixed = TRUE)
                return(outLocal)
            }))
            year2 <- unlist(as.integer(substr(TimePeriod, 6, 7)))
            ParsedUnitNames <- unlist(lapply(ParsedUnitNames, function(UnitName){
                
                outLocal <- gsub('[aa + 1]', year2 + 1, UnitName, fixed = TRUE)
                outLocal <- gsub('[aa]', year2, outLocal, fixed = TRUE)
                outLocal <- gsub('[aa - 1]', year2 - 1, outLocal, fixed = TRUE)
                return(outLocal)
            }))
        }))
    }
    
    return(output)
    }
)

#' @rdname ParseUnitName
#' 
#' @import data.table RepoTime
#' 
#' @export
setMethod(
    f = "ParseUnitName",
    signature = c("character", "integer"),
    function(UnitNames, MetaValues){
        
        IntNums <- MetaValues
        output <- unlist(lapply(IntNums, function(IntNum){
            
            outLocal <- gsub('[n]', IntNum, UnitNames, fixed = TRUE)
            return(outLocal)
            
        
        }))
        
        return(output)
    }
)

#' @rdname ParseUnitName
#' 
#' @import data.table RepoTime
#' 
#' @export
setMethod(
  f = "ParseUnitName",
  signature = c("character", "character"),
  function(UnitNames, MetaValues){
    
    IntChars <- MetaValues
    output <- unlist(lapply(IntChars, function(IntChars){
      
      outLocal <- gsub('[ccaa]', IntChars, UnitNames, fixed = TRUE)
      return(outLocal)
      
      
    }))
    
    return(output)
  }
)