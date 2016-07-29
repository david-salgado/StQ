#' @title Convert a dcasted \linkS4class{data.table} into an object of class
#' \linkS4class{StQ}
#'
#' @description \code{melt_StQ} returns an object of class \linkS4class{StQ}
#' from a \linkS4class{data.table} in dcasted form.
#'
#' This method builds an object of class \linkS4class{StQ} with the slot
#' \code{Data} constructed from the input \linkS4class{data.table} and the slot
#' \code{DD} given as an input parameter.
#'
#' This function can be considered as a constructor for the class \code{StQ}. It
#' is indeed a wrapper for the function \code{\link[data.table]{melt.data.table}}
#' from the package \linkS4class{data.table} adapted to the structure of the slot
#' \code{Data} of object \linkS4class{StQ}.
#'
#' The input parameter \code{DD} must be of class \linkS4class{DD}.
#'
#' @param DataMatrix of class \linkS4class{data.table} with dcasted form
#' (statistical units by rows and variables by columns).
#' 
#' @param DD Object of class \linkS4class{DD}.
#'
#' @return Object of class \linkS4class{StQ}.
#'
#' @examples
#' data(ExampleDM)
#' data(ExampleDD)
#' Q <- melt_StQ(ExampleDM, ExampleDD)
#' str(Q)
#'
#' @seealso \code{\link{dcast_StQ}}, \code{\link[data.table]{dcast.data.table}},
#' \code{\link[data.table]{melt.data.table}}, \code{\link[reshape2]{melt}},
#' \code{\link[reshape2]{dcast}}
#'
#' @include Datadt-class.R StQ-class.R ExtractNames.R getVNC.R
#'
#' @import data.table
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
    
    #Construimos un objeto DD auxiliar
    slots <- setdiff(slotNames(DD), 'VarNameCorresp')
    DDdtlocal <- new(Class = 'DDdt')

    for (slot in slots) {DDdtlocal <- DDdtlocal + slot(DD, slot)}

    
    nQual <- length(setdiff(names(DDdtlocal), c('Variable', 'Sort', 'Class', 'Length', 'ValueRegExp')))
    if (nQual == 0) stop('[StQ::melt_StQ] DD has no qualifiers.')

    auxDDdt <- copy(DDdtlocal)[, c('Variable', paste0('Qual', 1:nQual)), with = F]

    IDQual <- DDdtlocal[Sort == 'IDQual', Variable]
    NonIDQual <- DDdtlocal[Sort == 'NonIDQual', Variable]
    IDDD <- DDdtlocal[Sort == 'IDDD', Variable]

    # Calificadores ID, calificadores NonID y variables IDDD en la matriz de datos
    DM.Names <- names(DataMatrix)
    DM.IDQual <- DM.Names[DM.Names %in% IDQual]

    #DM.NonIDQual <- names(DataMatrix)
    DM.NonIDQual <- DM.Names[DM.Names %in% NonIDQual]

    DM.IDDD <- setdiff(ExtractNames(names(DataMatrix)), c(DM.IDQual, DM.NonIDQual))

    # Comprobamos que el DD es el que corresponde a la matriz de datos del input
    if (length(DM.IDQual) == 0 && length(DM.NonIDQual) == 0){
        
        stop(paste0('[StQ::melt_StQ] There is not consistency between the data.table and DD.'))
    }
    
    lapply(DM.IDDD, function(var){
        
        if (!var %in% IDDD){
            stop(paste0('[StQ::melt_StQ] Variable ', var, ' is not a variable in DD.'))
        }
    })
    

    
    # Generamos una data.table con una columna Qual que especifica los calificadores de cada variable de la matriz de entrada
    DM.Names <- list()
    for (DMiddd in DM.IDDD){

      DM.Names[[DMiddd]] <- names(DataMatrix)[grep(paste0('^', DMiddd, '$'),
                                            ExtractNames(names(DataMatrix)))]

    }
   
    auxDDdt <- auxDDdt[Variable %in% DM.IDDD]

    auxDDdt[, Qual := '']
    for (i in 1:nQual){

      auxDDdt[, Qual := ifelse(get(paste0('Qual', i)) != '',
                                       trim(paste(Qual, get(paste0('Qual',
                                                                   i)))),
                                       trim(Qual))]

    }
    auxDDdt <- auxDDdt[Qual1 != '']

    #auxDD <- rbindlist(auxDD, fill = TRUE)

    for (col in names(auxDDdt)){
        
        auxDDdt[, col := ifelse(is.na(get(col)), '', get(col)), with = F]
        
    }
   
  

    # Generamos una lista de data.tables que agrupen a las variables según sus calificadores
    auxMeasureVar <- split(auxDDdt[['Variable']], auxDDdt[['Qual']])

    moltenData <- lapply(as.list(names(auxMeasureVar)), function(QualName){

        auxVarNames <- unique(unlist(DM.Names[
            names(DM.Names) %in% intersect(DM.IDDD, auxMeasureVar[[QualName]])]))
        names(auxVarNames) <- NULL

        qual <- unlist(strsplit(QualName, ' '))
        qualinDM <- intersect(qual, names(DataMatrix))
        qualnotinDM <- setdiff(qual, names(DataMatrix))
       
        aux <- DataMatrix[, c(qualinDM, auxVarNames), with = F]
        

        for (col in names(aux)){

            aux[, col := as.character(get(col)), with = F]

        }
        
        out <- data.table::melt.data.table(aux,
                                          id.vars = qualinDM,
                                          measure.vars= auxVarNames,
                                          variable.name = 'IDDD',
                                          value.name = 'Value',
                                          variable.factor = FALSE,
                                          value.factor = FALSE)

        for (VNCcomp in names(getVNC(DD))){
            var <- auxMeasureVar[[QualName]]
            Excel <- DD@VarNameCorresp[[VNCcomp]]
            qualnotinDMinExcel <- intersect(qualnotinDM, names(Excel))
            varExcel <- Excel[IDDD %in% var]

            if (dim(varExcel)[1] > 0 && length(qualnotinDMinExcel) > 0){
                
                varExcel <- varExcel[, c('IDDD', qualnotinDMinExcel), with = FALSE]
                for (col in setdiff(names(varExcel), 'IDDD')){
                    varExcel[get(col) == '.', col := '', with = F]
                }
                ColNames <- names(varExcel)
                NotEmptyCols <- c()
                for (col in ColNames){
                    
                    if (!all(is.na(varExcel[[col]]) | varExcel[[col]] == '')) NotEmptyCols <- c(NotEmptyCols, col)
                    
                }
                varExcel <- varExcel[, NotEmptyCols, with = F]

                ColsNotUnit <- setdiff(names(varExcel), c('IDDD'))
                for (col in ColsNotUnit) {
                    
                    varExcel[, IDDD := paste(IDDD, get(col), sep = '_')]
                    
                } 
                if (length(intersect(out[['IDDD']], varExcel[['IDDD']])) == 0) next
                out <- merge(out, varExcel, by = 'IDDD')
                out[, IDDD := ExtractNames(IDDD)]
            }
        }
        
    setcolorder(out, c(qualinDM, intersect(names(out),qualnotinDM), 'IDDD', 'Value'))
    return(out)
    })

    names(moltenData) <- names(auxMeasureVar)

    # Incluimos las mismas columnas en cada componente de la lista
    ColNames <- unique(unlist(lapply(as.list(names(moltenData)),
                                     function(x){unlist(strsplit(x, ' ',
                                                                 fixed = T,
                                                                 useBytes = T))})))

    if (length(moltenData) > 1) {
        moltenData <- lapply(moltenData, function(moltenDT){

          for (NewCol in setdiff(ColNames, names(moltenDT))){

            moltenDT[, NewCol := '', with = F]

          }
          setcolorder(moltenDT, c(ColNames, 'IDDD', 'Value'))
          setkeyv(moltenDT, c(ColNames, 'IDDD', 'Value'))
          return(moltenDT)
        })
    }

    # Generamos el objeto StQ final
    output <- rbindlist(moltenData)
    output[is.nan(Value) | Value == 'NaN', Value := NA]
    setkeyv(output, setdiff(names(output), 'Value'))
    output <- output[!duplicated(output)]
    output <- new(Class = 'Datadt', output)
    
    output.StQ <- new(Class = 'StQ', Data = output, DD = DD)

    validObject(output.StQ)
    return(output.StQ)

}
