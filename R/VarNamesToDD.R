#' @title Return a reduced \linkS4class{DD} involving the input variable names
#'
#' @description \code{VarNamesToDD} returns a \linkS4class{DD} object including only the variable 
#' names specified in the input parameter \code{VarNames}.
#'
#' @param VarNames Character vector with the compound variable names.
#'
#' @param DD Object of class \linkS4class{DD} with the definition and properties of the variables.
#'
#' @return \linkS4class{DD} involving only the compound variable names specified in the input 
#' parameter \code{VarNames}.
#'
#'
#' @examples
#' data(ExampleDD)         
#'  
#' VarNames <- c('Turnover') 
#' VarNamesToDD(VarNames, ExampleDD)
#'  
#' VarNames <- c('NOrden', 'Turnover', 'EsRemuner', 'Employees')
#' VarNamesToDD(VarNames, ExampleDD)                     
#' 
#' @include ExtractNames.R setVNC.R getVNC.R DDdt-class.R
#'
#' @import data.table
#'
#' @export
VarNamesToDD <- function(VarNames, DD){
    
    outputDD <- new(Class = 'DD')
    setVNC(outputDD) <- getVNC(DD)

    # Para una sola variable
    if (is.character(VarNames) & length(VarNames) == 1) {
        
        DDSlotNames <- setdiff(slotNames(DD), 'VarNameCorresp')

        for (DDslot in DDSlotNames) {
            
            DDdtlocal <- slot(DD, DDslot)
            Names.DT <- DDdtlocal[Variable == ExtractNames(VarNames)]

            if (dim(Names.DT)[1] != 0) {
                QualNames <- setdiff(names(Names.DT), 
                                    c('Variable', 'Sort', 'Class', 'Length', 'Qual1', 'ValueRegExp'))
                for (col in QualNames) {
                    
                    if (all(Names.DT[[col]] == '')) {
                        Names.DT[, col := NULL, with = F]
                    }
                    
                }
                ColNames.DT <- names(Names.DT)
                nQual <- length(grep('Qual', ColNames.DT)) 
                
                if (nQual >1) {
                    setnames(Names.DT, c('Variable', 'Sort', 'Class', 'Length', paste0('Qual', 1:nQual), 'ValueRegExp'))
                }else {
                    setnames(Names.DT, c('Variable', 'Sort', 'Class', 'Length', 'Qual1', 'ValueRegExp'))
                }
            }

            #Construimos el objecto DD por Slots
            if (DDslot == 'ID') {
                
                outputDD@ID <- new(Class = 'DDdt', Names.DT)

            } else if (DDslot == 'MicroData') {
                
                outputDD@MicroData <- new(Class = 'DDdt', Names.DT)
                
                
            } else if (DDslot == 'ParaData') {
                
                outputDD@ParaData <- new(Class = 'DDdt', Names.DT)
                
            } else if (DDslot == 'Aggregates') {
                
                outputDD@Aggregates <- new(Class = 'DDdt', Names.DT)
                
            } else if (DDslot == 'AggWeights') {
                
                outputDD@AggWeights <- new(Class = 'DDdt', Names.DT)
                
            } else {
                
                outputDD@Other <- new(Class = 'DDdt', Names.DT)
                
            }
            
        }
     
        return(outputDD)
        
    } else {# Ahora para varias variables de entrada
        
        out.list <- lapply(as.list(VarNames), VarNamesToDD, DD = DD)
        
        out <- Reduce(`+`, out.list, out.list[[1L]])
        
        return(out)
        
        
        #Split <- strsplit(VarNames,'_')
        #Raiz <- c()
        #for(i in 1:length(Split)){
        #    Raiz[i] <- Split[[i]][1]
        #}
        #Raiz <- Raiz[!duplicated(Raiz)]
        
        #VarRaiz <- c()
        #for (Raiz_i in Raiz){
        #   L <- c()
        #   for (i in 1:length(Split)){
        #       if (Split[[i]][1] == Raiz_i) {L <- c(L, as.vector(Split[[i]]))}
        #   }
        #   L <- L[!duplicated(L)] 
        #   H <- L[1]
        #   if (length(L)>1){
        #   for (i in 2:length(L)){H <- paste0(H, sep = '_', L[i])} }
        #   VarRaiz <- c(VarRaiz,H)
        #}
        
        #outputDD <- new(Class='DD')

        #for (Var in VarRaiz){
        #    outputDD <- outputDD + VarNamesToDD(Var,DD)
        #}
        
        #return(outputDD)
       
    }
}
