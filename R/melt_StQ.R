#' @title Convert a dcasted \linkS4class{data.table} into an object of class \linkS4class{StQ}
#'
#' @description \code{melt_StQ} returns an object of class \linkS4class{StQ} from a
#' \linkS4class{data.table} in dcasted form.
#'
#' This method builds an object of class \linkS4class{StQ} with the slot \code{Data} constructed
#' from the input \linkS4class{data.table} and the slot \code{DD} given as an input parameter.
#'
#' This function can be considered as a constructor for the class \code{StQ}. It is indeed a wrapper
#'  for the function \code{\link[data.table]{melt.data.table}} from the package
#'  \linkS4class{data.table} adapted to the structure of the slot \code{Data} of object
#'  \linkS4class{StQ}.
#'
#' The input parameter \code{DD} must be of class \linkS4class{DD}.
#'
#' @param DataMatrix of class \linkS4class{data.table} with dcasted form (statistical units by rows
#' and variables by columns).
#'
#' @param DD Object of class \linkS4class{DD}.
#'
#' @return Object of class \linkS4class{StQ}.
#'
#' @examples
#' data(ExampleDM)
#' data(ExampleDD)
#' melt_StQ(ExampleDM, ExampleDD)
#'
#'
#' @seealso \code{\link{dcast_StQ}}, \code{\link[data.table]{dcast.data.table}},
#' \code{\link[data.table]{melt.data.table}}, \code{\link[reshape2]{melt}},
#' \code{\link[reshape2]{dcast}}
#'
#' @include StQ.R ExtractNames.R getVNC.R UnitToIDDDNames.R
#'
#' @import data.table
#'
#' @importFrom stringi stri_split_fixed
#'
#' @export
melt_StQ <- function(DataMatrix, DD){

    # Función que elimina carácter blanco al principio y al final
    trim <- function (x) gsub("^\\s+|\\s+$", "", x, useBytes = T)

    # Función para construir nombres de variables
    pasteNA <- function(x, y){

        out <- ifelse(is.na(y) | y == '', paste0(x, ''), paste(x, y, sep = "_"))
        return(out)

    }

    DM <- copy(DataMatrix)
    namesDM <- names(DM)
    if (!all(ExtractNames(namesDM) %in% getVariables(DD))) {
      
      newNamesDM <- UnitToIDDDNames(namesDM, DD)
      if (any(is.null(newNamesDM))) stop('[StQ::melt_StQ] Column names not identified in the DD object.')
      setnames(DM, newNamesDM)  
    }
    

    IDQuals <- intersect(getIDQual(DD), namesDM)
    NonIDQuals <- getNonIDQual(DD)
    dotQuals <- intersect(getDotQual(DD), namesDM)
    dotdotQuals <- intersect(getDoubleDotQual(DD), namesDM)
    IDDDs <- intersect(getIDDD(DD), ExtractNames(namesDM))
    slots <- setdiff(names(getVNC(DD)), 'VarSpec')
    auxDDDT <- lapply(slots, function(VNCname){
      
      DDslot <- ExtractNames(VNCname)
      auxDDdt <- get(paste0('get', DDslot))(DD)
      nQual <- length(grep('Qual', names(auxDDdt)))
      auxDDdt <- auxDDdt[, c('Variable', paste0('Qual', 1:nQual)), with = F]
      auxDDdt <- auxDDdt[Variable %in% IDDDs]
      auxDDdt[, Qual := '']
      return(auxDDdt)
    })
    auxDDDT <- rbindlist(auxDDDT)
    auxDDDT <- auxDDDT[!duplicated(auxDDDT)]
    nQual <- length(grep('Qual', names(auxDDDT))) - 1
    for (i in 1:nQual){
      
      auxDDDT[, Qual := ifelse(get(paste0('Qual', i)) != '',
                               trim(paste(Qual, get(paste0('Qual',i)))),
                               trim(Qual))]
      
    }
    auxMeasureVar <- split(auxDDDT[['Variable']], auxDDDT[['Qual']])

    moltenData <- lapply(seq_along(auxMeasureVar), function(index.auxMeasureVar){
      
      QualName <- names(auxMeasureVar)[index.auxMeasureVar]
      indexCol <- ExtractNames(namesDM) %in% auxMeasureVar[[QualName]]
      localQuals <- strsplit(QualName, ' ')[[1]]
      ColNames <- c(localQuals, names(DM)[indexCol])
      localDM <- DM[, intersect(ColNames, namesDM), with = F]
      localDM[, lapply(.SD, as.character), by = IDQuals]
      localID <- intersect(unique(c(IDQuals, dotQuals)), localQuals)
      out <- data.table::melt.data.table(localDM,
                                         id.vars = localID,
                                         measure.vars= setdiff(names(localDM), localID),
                                         variable.name = 'IDDD',
                                         value.name = 'Value',
                                         variable.factor = FALSE,
                                         value.factor = FALSE)

      out <- out[Value != '']

      localNonIDQual <- strsplit(QualName, ' ', fixed = TRUE)[[1]]
      localNonIDQual <- setdiff(localNonIDQual, c(IDQuals, dotQuals, dotdotQuals))

      if (dim(out)[1] == 0) {
        
        return(data.table(NULL))
        
      } else {
        
        if (length(localNonIDQual) != 0){

          colNames <- c(auxMeasureVar[[QualName]], localNonIDQual)
          outLocal <- out[,  tstrsplit(IDDD, '_', fixed = TRUE, fill = '')]
          if (length(colNames) == dim(outLocal)[2] + 1) outLocal[, (colNames[length(colNames)]) := '']
          setnames(outLocal, colNames)
          outLocal <- out[, (names(outLocal)) := outLocal][, IDDD := NULL]
          setnames(outLocal, auxMeasureVar[[QualName]], 'IDDD')
          localdotQuals <- intersect(dotQuals, names(outLocal))
          localdotdotQuals <- intersect(dotdotQuals, names(outLocal))
          setcolorder(outLocal, unique(c(localID, localNonIDQual, localdotQuals, localdotdotQuals,  'IDDD', 'Value')))

        } else {
          
          outLocal <- out
          
        }
        
        return(outLocal)  
      }
    })

    names(moltenData) <- names(auxMeasureVar)
    moltenData <- rbindlist(moltenData, fill = TRUE)
    
    if (all(dim(moltenData) == c(0, 0))) {
      
      output.StQ <- StQ()
      
    } else {
      
      moltenData[is.nan(Value) | Value == 'NaN', Value := '']
      setkeyv(moltenData, setdiff(names(moltenData), 'Value'))
      moltenData <- moltenData[!duplicated(moltenData, by = key(moltenData))]
      setcolorder(moltenData, c(setdiff(names(moltenData), c('Value', 'IDDD')), 'IDDD', 'Value'))
      ColNames <- names(moltenData)
      for (col in ColNames){
        
        moltenData[is.na(get(col)), (col) := '']
      }
      
      output.StQ <- StQ(Data = moltenData, DD = DD)
      
    }
    return(output.StQ)
    
}
