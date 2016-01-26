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
#' @param DataMatrix \linkS4class{data.table} with dcasted form (statistical
#' units by rows and variables by columns).
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
#' @include StQ-class.R DD-class.R getDD.R getData.R getUnits.R ExtractNames.R
#'
#' @import data.table
#'
#' @export
melt_StQ <- function(DataMatrix, DD){

    # Función que elimina carácter blanco al principio y al final
    trim <- function (x) gsub("^\\s+|\\s+$", "", x, useBytes = T)

    # Función que divide por el guión bajo _ y escoge el componente n
    StrSplit_ <- function(x, n){

        x <- ifelse(substr(x, nchar(x), nchar(x)) == '_', paste0(x,' '), x)
        out <- strsplit(x, "_", fixed = T, useBytes = T)
        out <- lapply(out, function(VarVector){
                              if(length(VarVector) <= n) {

                                return(VarVector[n])

                              } else {

                                return(c(VarVector,
                                         rep('', length(VarVector) - n))[n])
                              }
          })
        out <- unlist(out)
        return(out)

    }

    #Construimos un objeto DD auxiliar
    auxDD <- list()
    for (DDslot in setdiff(slotNames(DD), 'VarNameCorresp')){

        DDlocal <- slot(DD, DDslot)
        nQual <- length(setdiff(names(DDlocal), c('Variable', 'Sort', 'Class')))
        if (nQual == 0) stop('[StQ::melt_StQ] DD has no qualifiers.')

        auxDD[[DDslot]] <- copy(DDlocal)[, c('Variable',
                                             paste0('Qual', 1:nQual)), with = F]

        IDQual <- DDlocal[Sort == 'IDQual', Variable]
        NonIDQual <- DDlocal[Sort == 'NonIDQual', Variable]
        IDDD <- DDlocal[Sort == 'IDDD', Variable]

        # Calificadores ID, calificadores NonID y variables IDDD en la matriz de datos
        DM.IDQual <- names(DataMatrix)
        DM.IDQual <- DM.IDQual[DM.IDQual %in% IDQual]

        DM.NonIDQual <- names(DataMatrix)
        DM.NonIDQual <- DM.NonIDQual[DM.NonIDQual %in% NonIDQual]

        DM.IDDD <- setdiff(ExtractNames(names(DataMatrix)),
                           c(DM.IDQual, DM.NonIDQual))

        # Generamos una data.table con una columna Qual que especifica los calificadores de cada variable de la matriz de entrada
        DM.Names <- list()
        for (DMiddd in DM.IDDD){

          DM.Names[[DMiddd]] <- names(DataMatrix)[grep(paste0('^', DMiddd, '$'),
                                                ExtractNames(names(DataMatrix)))]

        }

        auxDD[[DDslot]] <- auxDD[[DDslot]][Variable %in% DM.IDDD]

        auxDD[[DDslot]][, Qual := '']
        for (i in 1:nQual){

          auxDD[[DDslot]][, Qual := ifelse(get(paste0('Qual', i)) != '',
                                           trim(paste(Qual, get(paste0('Qual',
                                                                       i)))),
                                           trim(Qual))]

        }
        auxDD[[DDslot]] <- auxDD[[DDslot]][Qual1 != '']

    }

    auxDD <- rbindlist(auxDD, fill = TRUE)
    
    for (col in names(auxDD)){
        
        auxDD[, col := ifelse(is.na(get(col)), '', get(col)), with = F]
        
    }

    # Generamos una lista de data.tables que agrupen a las variables según sus calificadores
    auxMeasureVar <- split(auxDD[['Variable']], auxDD[['Qual']])

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

        for (qualNonID.index in seq(along = qualnotinDM)){

          out[, qualnotinDM[qualNonID.index] :=
                  StrSplit_(IDDD, 1 + qualNonID.index), with = F]
          out[, qualnotinDM[qualNonID.index] :=
                  as.character(get(qualnotinDM[qualNonID.index])), with = F]
          out[, qualnotinDM[qualNonID.index] :=
                  ifelse(is.na(get(qualnotinDM[qualNonID.index])),
                         '',
                         get(qualnotinDM[qualNonID.index])), with = F]

        }
        out[, IDDD := ExtractNames(IDDD)]
        setcolorder(out, c(qualinDM, qualnotinDM, 'IDDD', 'Value'))
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

    output.StQ <- new(Class = 'StQ', Data = output, DD = DD)
    validObject(output.StQ)
    return(output.StQ)

}
