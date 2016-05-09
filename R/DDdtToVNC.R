#' @title Returns the corresponding object of class \linkS4class{VarNameCorresp} according to the
#'  DDdt input object.
#'
#' @description \code{DDdtToVNC} returns the corresponding object of class 
#' \linkS4class{VarNameCorresp} according to the DDdt input object.
#'
#' @param DDdt Object of class \linkS4class{DDdt}.
#' 
#' @param NameVNC character vector with the name of the element in VNC which is being build.
#'
#' @return the corresponding object of class \linkS4class{VarNameCorresp}.
#'
#' @examples
#' library(data.table)
#' AggWeights <- data.table(Variable = c('ID', 'Pond1'), Sort = c('IDQual', 'IDDD'),
#'                          Class = c('character', 'character'),
#'                          Qual1 = c('', 'ID'), ValueRegExp = c('', ''))
#' AggWeights <- new(Class = 'DDdt', AggWeights)
#' 
#' VNC <- DDdtToVNC(AggWeights, 'AggWeights')
#' 
#' @include DDdt-class.R 
#'
#' @import data.table
#'
#' @export
DDdtToVNC <- function(DDdt, NameVNC){
    
    numVar <- length(DDdt[['Variable']])
    IDQual <- vector('character', numVar)
    NonIDQual <- vector('character', numVar)
    IDDD <- vector('character', numVar)
    Unit1 <- vector('character', numVar)
        
    IDQualdt <- DDdt[Sort == 'IDQual', Variable]
    NonIDQualdt <- DDdt[Sort == 'NonIDQual', Variable]
    IDDDdt <- DDdt[Sort == 'IDDD', Variable]
    
    if (length(IDQualdt) > 0){
        
        fin <- length(IDQualdt)
        IDQual[1:fin] <- IDQualdt
    }else{
        
        fin <- 0
    }

    if (length(NonIDQualdt) > 0){
        
        init <- fin + 1
        fin <- fin + length(NonIDQualdt) 
        NonIDQual[init:fin] <- NonIDQualdt
    }

    if (length(IDDDdt) > 0){
        
        IDDD[(fin + 1): (fin + length(IDDDdt))] <- IDDDdt
    }
    
    output <- data.table(IDQual, NonIDQual, IDDD)
    Quals <- c(IDQualdt, NonIDQualdt)
    for (i in seq(1,length(Quals))){
        
        output <- cbind(output, vector('character', numVar))
        setnames(output, setdiff(names(output), c('IDQual', 'NonIDQual', 'IDDD'))[i], Quals[i])
    }

    
    output <- cbind(output, Unit1)
    output <- new(Class = 'VNCdt', output)
    output <- list(output)
    names(output) <- NameVNC
    output <- BuildVNC(output)
    
    return(output)
    
}
