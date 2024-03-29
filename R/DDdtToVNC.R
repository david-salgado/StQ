#' @title Returns the corresponding object of class \link{VNC} according to the input 
#' \linkS4class{data.table}
#'
#' @description \code{DDdtToVNC} returns the corresponding object of class 
#' \link{VNC} according to the input \linkS4class{data.table}.
#'
#' @param DDdt Object of class \linkS4class{data.table}.
#'
#' @param NameVNC character vector with the name of the element in VNC which is being build.
#'
#' @param InFiles character vector with as many components as the rows of \code{DDdt} specifying the 
#' files where to include each variable.
#'
#' @return Returns the corresponding object of class \linkS4class{VarNameCorresp}.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' AggWeights <- data.table(Variable = c('ID', 'Pond1'), Sort = c('IDQual', 'IDDD'),
#'                          Class = c('character', 'character'),
#'                          Length = c('11', '7'),
#'                          Qual1 = c('', 'ID'), ValueRegExp = c('', ''))
#' DDdtToVNC(AggWeights, 'AggWeights', rep('FA', 2))
#' }
#'
#' @include VNC.R BuildVNC.R DD.R BuildDD.R getIDQual.R getNonIDQual.R getIDDD.R
#'
#' @import data.table
#'
#' @export
DDdtToVNC <- function(DDdt, NameVNC, InFiles = rep('', dim(DDdt)[1])){
    
    Sort <- NULL
  
    #auxVarList_DD <- list(VNC = VNC(), New = DDdt)
    #names(auxVarList_DD) <- c('VNC', NameVNC)
    #newDD <- try(BuildDD(auxVarList_DD))
    #if (inherits(newDD, "try-error")) stop('[StQ::DDdtToVNC] DDdt does not have the correct format.\n')
    
    
    numVar <- length(DDdt[['Variable']])
    if (length(InFiles) == 1) InFiles <- rep(InFiles, numVar)
    if (length(InFiles) != numVar) stop('[StQ::DDdtToVNC] InFiles must be a character with as many components as rows in DDdt.\n')
    IDQual <- vector('character', numVar)
    NonIDQual <- vector('character', numVar)
    IDDD <- vector('character', numVar)
    UnitName <- vector('character', numVar)

    IDQualdt <- DDdt[Sort == 'IDQual'][['Variable']]
    IDQualdt <- IDQualdt[IDQualdt != '']

    NonIDQualdt <- DDdt[Sort == 'NonIDQual'][['Variable']]
    NonIDQualdt <- NonIDQualdt[NonIDQualdt != '']
    IDDDdt <- DDdt[Sort == 'IDDD'][['Variable']]
    IDDDdt <- IDDDdt[IDDDdt != '']
    
    if (length(IDQualdt) > 0){

        fin <- length(IDQualdt)
        IDQual[1:fin] <- IDQualdt

    } else {

        fin <- 0
    }

    if (length(NonIDQualdt) > 0){

        init <- fin + 1
        fin <- fin + length(NonIDQualdt)
        NonIDQual[init:fin] <- NonIDQualdt
    }

    if (length(IDDDdt) > 0){

        IDDD[(fin + 1):(fin + length(IDDDdt))] <- IDDDdt
    }

    output <- data.table(IDQual, NonIDQual, IDDD)

    Quals <- c(IDQualdt, NonIDQualdt)
    if (length(Quals) >= 1){

      for (i in seq(1, length(Quals))){

        output <- cbind(output, vector('character', numVar))
        setnames(output, setdiff(names(output), c('IDQual', 'NonIDQual', 'IDDD'))[i], Quals[i])
      }

      output[['TipoMicrodato']] <- c(rep('05.', length(Quals)), rep('', numVar - length(Quals)))
    }

    VarList_output <- cbind(output, UnitName)
    VarList_output <- cbind(VarList_output, InFiles)
    VarList_VNC <- list(Name = VarList_output)
    names(VarList_VNC) <- NameVNC
    output <- BuildVNC(VarList_VNC)

    return(output)

}
