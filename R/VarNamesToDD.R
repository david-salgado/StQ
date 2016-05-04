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
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = new(Class = 'VNCdt',data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                                     NonIDQual = c('','','','',''),
#'                                                     IDDD = c('', 'Name', 'Surname', 'PostalAddr',
#'                                                              'PhoneNo'),
#'                                                     NumIdEst = c('', rep('.', 4)),
#'                                                     Unit1 = c('numidest', 'nombre', 'apellidos', 
#'                                                               'direccion', 'telefono')     
#' )),
#' MicroData =new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                            NonIDQual = c('', 'IsNatMarket', 
#'                                                          'IsEuroMarket', 
#'                                                          'IsRWMarket',
#'                                                          ''),
#'                                            IDDD = c(rep('', 4), 'NewOrders'),
#'                                            NumIdEst = c(rep('', 4), '.'),
#'                                            IsNatMarket = c(rep('', 4), '0'),
#'                                            IsEuroMarket = c(rep('', 4), '0'),
#'                                            IsRWMarket = c(rep('', 4), '1'),
#'                                            Unit1 = c('numidest', rep('', 3), 'cp09'))),
#' ParaData = new(Class = 'VNCdt',data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                           NonIDQual = c('', 'Action', ''),
#'                                           IDDD = c(rep('', 2), 'Date'),
#'                                           NumIdEst = c(rep('', 2), '.'),
#'                                           Action = c(rep('', 2), 'Imputation'),
#'                                           Unit1 = c('numidest', '', 'FechaImput'))))
#' 
#' VNC <- new(Class = 'VarNameCorresp', VarList)
#' 
#' ### We build the specification data.tables
#' IDdt <- new( Class='DDdt',data.table(
#'     Variable = c('NOrden','NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'     Sort = c('IDQual','IDQual', rep('IDDD', 4)),
#'     Class = rep('character', 6),
#'     Qual1 = c('EsRemuner', '', rep('NumIdEst', 4)),
#'     Qual2 = c('Turnover', '', rep('', 4)),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '.+', '(6|9)[0-9]{8}')))
#' Microdt <- new( Class='DDdt',data.table(
#'     Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 
#'                  'IsRWMarket', 'NewOrders'),
#'     Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'     Class = c(rep('character', 4), 'numeric'),
#'     Qual1 = c(rep('', 4), 'NumIdEst'),
#'     ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), '([0-9]{1, 10}| )')
#' ))
#' Paradt <-new( Class='DDdt', data.table(
#'     Variable = c('NumIdEst', 'Action', 'Date'),
#'     Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'     Class = rep('character', 3),
#'     Qual1 = c(rep('', 2), 'NumIdEst'),
#'     Qual2 = c(rep('', 2), 'Action'),
#'     ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation', 
#'                     '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )')
#' ))
#' Aggdt <- new( Class='DDdt',data.table(
#'     Variable = c('Province', 'NACE09', 'Turnover'),
#'     Sort = c(rep('IDQual', 2), 'IDDD'),
#'     Class = c(rep('character', 2), 'numeric'),
#'     Qual1 = c(rep('', 2), 'Province'),
#'     Qual2 = c(rep('', 2), 'NACE09'),
#'     ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )')
#' ))
#' 
#' DD <- new(Class = 'DD', 
#'           VarNameCorresp = VNC, 
#'           ID = IDdt, 
#'           MicroData = Microdt, 
#'           ParaData = Paradt,
#'           Aggregates = Aggdt)
#'           
#'  
#' VarNames1 = c('NOrden_Turnover') 
#' VarNames2 = c('NOrden', 'NOrden_Turnover','Name','NOrden_EsRemuner')
#' VarNamesToDD(VarNames1, DD)        
#' VarNamesToDD(VarNames2, DD)                     
#' @include ExtractNames.R
#'
#' @import data.table
#'
#' @export
VarNamesToDD <- function(VarNames, DD){
    
    outputDD <-new(Class='DD')
    # Para una sola variable
    if (is.character(VarNames) & length(VarNames) == 1){
        
        DDSlotNames <- setdiff(slotNames(DD), 'VarNameCorresp')

        for (DDslot in DDSlotNames){
            
            DDdtlocal <- slot(DD, DDslot)
            Names.DT <- DDdtlocal[Variable == ExtractNames(VarNames)]
            
            if (dim(Names.DT)[1] != 0) {
                ColNames <- setdiff(names(Names.DT), 
                                    c('Variable', 'Sort', 'Class', 'Qual1', 'ValueRegExp'))
                for (col in ColNames){
                    
                    if (all(Names.DT[[col]] == '')) {
                        Names.DT[, col := NULL, with = F]
                    }
                    
                }
            }
                
            #Construimos el objecto DD por Slots
            if (DDslot=='ID'){
                
                outputDD@ID <- new(Class='DDdt', Names.DT)
                
            } else if (DDslot=='MicroData'){
                
                outputDD@MicroData <- new(Class='DDdt', Names.DT)
                
            } else if (DDslot=='ParaData'){
                
                outputDD@ParaData <- new(Class='DDdt', Names.DT)
                
            } else if (DDslot=='Aggregates'){
                
                outputDD@Aggregates <- new(Class='DDdt', Names.DT)}
            
            else if (DDslot=='AggWeights'){
                
                outputDD@AggWeights <- new(Class='DDdt', Names.DT)
                
            }else{
                
                outputDD@Other <- new(Class='DDdt', Names.DT)
                
            }
            
        }
     
        return(outputDD)
        
    } else { # Ahora para varias variables de entrada
        
        Split=strsplit(VarNames,'_')
        Raiz=c()
        for(i in 1:length(Split)){
            Raiz[i] <- Split[[i]][1]
        }
        Raiz <- Raiz[!duplicated(Raiz)]
        
        VarRaiz <- c()
        for (Raiz_i in Raiz){
           L <- c()
           for (i in 1:length(Split)){
               if (Split[[i]][1] == Raiz_i) {L <- c(L, as.vector(Split[[i]]))}
           }
           L <- L[!duplicated(L)] 
           H <- L[1]
           if (length(L)>1){
           for (i in 2:length(L)){H <- paste0(H,sep='_',L[i])} }
           VarRaiz <- c(VarRaiz,H)
        }
        
        outputDD <- new(Class='DD')
        for (Var in VarRaiz){
            outputDD <- outputDD + VarNamesToDD(Var,DD)
        }
        
        return(outputDD)
       
    }
}
