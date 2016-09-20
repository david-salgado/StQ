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
    setnames(DM, names(DM), UnitToIDDDNames(names(DM), DD))

    #Construimos un objeto DD auxiliar
    slots <- setdiff(names(getVNC(DD)), 'VarSpec')

    out <- lapply(slots, function(VNCName){
        
        DDdtlocal <- slot(DD, ExtractNames(VNCName))
        VNClocal <- getVNC(DD)[[VNCName]]
        nQual <- length(grep('Qual', names(DDdtlocal)))
        auxDDdt <- copy(DDdtlocal)[, c('Variable', paste0('Qual', 1:nQual)), with = F]
        IDQual <- DDdtlocal[Sort == 'IDQual', Variable]
        NonIDQual <- DDdtlocal[Sort == 'NonIDQual', Variable]
        IDDD <- DDdtlocal[Sort == 'IDDD', Variable]
    
        # Calificadores ID, calificadores NonID y variables IDDD en la matriz de datos
        DM.Names <- names(DM)
        DM.IDQual <- DM.Names[DM.Names %in% IDQual]
        DM.NonIDQual <- DM.Names[DM.Names %in% NonIDQual]

        #DM.IDDD <- setdiff(intersect(ExtractNames(DM.Names), IDDD), c(DM.IDQual, DM.NonIDQual))
        DM.IDDD <- setdiff(DM.Names, c(DM.IDQual, DM.NonIDQual))

        auxDDdt <- auxDDdt[Variable %in% ExtractNames(DM.IDDD)]

        auxDDdt[, Qual := '']
        for (i in 1:nQual){
            
            auxDDdt[, Qual := ifelse(get(paste0('Qual', i)) != '',
                                     trim(paste(Qual, get(paste0('Qual',
                                                                 i)))),
                                     trim(Qual))]
            
        }
        #auxDDdt <- auxDDdt[Qual1 != '']

        ColNames <- names(auxDDdt)
        for (col in ColNames){
            
            auxDDdt[, col := ifelse(is.na(get(col)), '', get(col)), with = F]
            
        }

        auxMeasureVar <- split(auxDDdt[['Variable']], auxDDdt[['Qual']])
        if (length(auxMeasureVar) == 0) return(data.table(NULL))
#        auxMeasureVar <- auxMeasureVar[which(lapply(auxMeasureVar, length) > 0)]
        

        moltenData <- lapply(names(auxMeasureVar), function(QualName){
            
            indexCol <- ExtractNames(names(DM)) %in% auxMeasureVar[[QualName]]
            LocalQuals <- strsplit(QualName, ' ')[[1]]
            ColNames <- c(LocalQuals, names(DM)[indexCol])
            localDM <- DM[, intersect(ColNames, names(DM)), with = F]

            for (col in names(localDM)){
                
                localDM[, col := as.character(get(col)), with = F]
                
            }
            IDQual <- intersect(IDQual, names(localDM))

            out <- data.table::melt.data.table(localDM,
                                               id.vars = IDQual,
                                               measure.vars= setdiff(names(localDM), IDQual),
                                               variable.name = 'IDDD',
                                               value.name = 'Value',
                                               variable.factor = FALSE,
                                               value.factor = FALSE)

            out <- out[Value != '']
            LocalNonIDQual <- setdiff(LocalQuals, IDQual)
            if (dim(out)[1] != 0){
                
                outLocal <- stringi::stri_split_fixed(out[['IDDD']], '_')

                if (length(outLocal) == 1) {
                    
                    outLocal <- as.data.table(t(as.matrix(outLocal[[1]])))
                    
                } else {
                    
                    outLocal <- as.data.table(Reduce(rbind, outLocal))
                    
                }
                setnames(outLocal, c('IDDD', LocalNonIDQual))
                outLocal[, Value := out[['Value']]]
                for (idqual in IDQual){
                    
                    outLocal[, idqual := out[[idqual]], with = F]
                }
                setcolorder(outLocal, c(IDQual, LocalNonIDQual, 'IDDD', 'Value'))
            
            } else {
                
                outLocal <- data.table(NULL)
                
            }
            return(outLocal)

        })

        names(moltenData) <- names(auxMeasureVar)

        #moltenData <- lapply(moltenData, function(DT) { DT <- DT[get(unlist(strsplit(names(DT), ' '))) != ""]})
        
        moltenData <- rbindlist(moltenData, fill = TRUE)
        
        return(moltenData)
        
    })
   
    out <- rbindlist(out, fill = TRUE)
    
    if (all(dim(out) == c(0, 0))) {
        
        output.StQ <- new(Class = 'StQ')
    
    } else {
    
        out[is.nan(Value) | Value == 'NaN', Value := '']
        setkeyv(out, setdiff(names(out), 'Value'))
        out <- out[!duplicated(out)]
        setcolorder(out, c(setdiff(names(out), c('Value', 'IDDD')), 'IDDD', 'Value'))
        ColNames <- names(out)
        for (col in ColNames){
            
            out[is.na(get(col)), col := '', with = F]
        }
        
        
        out <- new(Class = 'Datadt', out)
        
        output.StQ <- new(Class = 'StQ', Data = out, DD = DD)
    
    }
    return(output.StQ)
}
