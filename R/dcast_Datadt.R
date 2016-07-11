#' @title Convert an \linkS4class{Datadt} object into a dcasted data.table
#'
#' @description \code{dcast_Datadt} returns a \linkS4class{data.table} in dcasted
#' form (observations by row and variables by columns) with data from the input
#' \linkS4class{Datadt} object.
#'
#' This methods converts the input \code{Datadt} object
#'  with key-value pair structure into a \linkS4class{data.table} with
#'  statistical units by row and variables specified in the input parameter
#'  \code{VarNames} by columns.
#'
#' @param object Object of class \linkS4class{Datadt}.  
#'
#' @param VarNames Character vector with names of the output variables.
#'
#'
#' @return \linkS4class{data.table} with data of the input
#' \linkS4class{Datadt} object with statistical units by rows and variables by
#'  columns. Only variables in \code{VarNames} will be output. If no variable
#'  name is specified, all variables in the input object will be output.
#'
#' @examples
#' library(data.table)
#' Ddt <- new(Class = 'Datadt', 
#'             data.table(ID = c('001', '001', '001', '001'), 
#'                        Market = c('0.', '1.', '', ''),
#'                        EsRemuner = c('2.2.','2.1.','',''),
#'                        IDDD = c('Turnover', 'Turnover', 'Province', 'NACE09'),
#'                        Value = c('625000', '23154', '04', '0512')))
#'                        
#' VarNames <- c('Turnover_0_1')
#' dcast_Datadt(Ddt,VarNames)
#' 
#' VarNames <- c('Turnover')
#' dcast_Datadt(Ddt,VarNames)
#' 
#' dcast_Datadt(Ddt)
#'
setGeneric("dcast_Datadt",
           function(object,
                    VarNames){standardGeneric("dcast_Datadt")})
#' @rdname dcast_Datadt
#'
#' @importFrom formula.tools lhs.vars
#'
#' @import data.table
#'
#' @include StQ-class.R getDD.R getData.R getUnits.R plus.DD.R VarNamesToFormula.R
#'
#' @export
setMethod(
    f = "dcast_Datadt",
    signature = c("Datadt"),
    function(object, VarNames){

        #Construimos todas las variables 
        TVar <- object$IDDD
        nQual <- length(names(object)) - 3
        if (nQual > 0){
            for (i in 1:nQual) {TVar <- data.table(TVar, object[[i+1]])}
            TVarS <- TVar[[1]]
            for (i in 1:nQual) {TVarS <- paste0(TVarS, '_', TVar[[i+1]])}
            # Limpiamos TVarS de los '_' que sobran
            for (i in 1:length(TVarS)){
              while (substr(TVarS[i], nchar(TVarS[i]), nchar(TVarS[i])) == '_') {
                  TVarS[i] <- substr(TVarS[i], 1, nchar(TVarS[i]) - 1)}
            
              while (regexpr('__', TVarS[i])!=-1) {
                  l1 <- regexpr('__', TVarS[i])[1]
                  l2 <- attr(regexpr('__', TVarS[i]),"match.length")
                  TVarS[i] <- paste0(substr(TVarS[i],1,l1-1),'_',substr(TVarS[i],l1+l2,nchar(TVarS[i])))
              }
           }
        
            TVarS <- TVarS[!duplicated(TVarS)]
        } else {TVarS <- TVar}
        
        if (missing(VarNames)) {VarNames <- TVarS}
        #Substrings
        cont <- 1
        VarNamesAux <- c()
        for (i in 1:length(VarNames)){
            R <- regexpr(VarNames[i], TVarS)
            Rn <- sum(R>(-1))
            if (Rn>0) {
                    VarNamesAux <- c(VarNamesAux, TVarS[R>(-1)]) 
            }
        }
        
        VarNames <- copy(VarNamesAux)
        Vardt <- intersect(VarNames,TVarS)
        if (length(Vardt)==0) stop('[Datadt::dcast_Datadt] Some introduced variables are not in the Datadt object.')
        
        Units <- getUnits(object)[[1]]
        output <- data.table(matrix(ncol=length(Vardt)+1,nrow=length(Units)))
        names(output) <- c(names(object)[1], Vardt)
        output[[1]] <- Units
        
        # Colocamos los valores
        List <- strsplit(Vardt, '_')
        Vect <- unlist(lapply(List, length))
        Max <- nQual + 1

        for (i in 1:length(List)){if (Vect[i] < Max) {List[[i]][(Vect[i] + 1):Max] <- '' }}
        
        ExNames <- ExtractNames(Vardt)
        Obj <- object[IDDD %in% ExNames]
        for (i in 1:length(Obj[[1]])){
            
            IDQual <- Obj[[1]][i]
            VarUs <- list(Obj[[nQual+2]][i])
            if (nQual > 0){for (j in 1:nQual) {VarUs[j+1] <- Obj[[j+1]][i]}}
            key <- 0
            cont <- 1
            for (j in 1:length(List)){
                
                if (prod(List[[j]] == VarUs) == 1) {
                    
                    key <- 1 
                    break
                    
                } else {
                    cont <- cont +1
                }
            }
            if (key == 1) {
            Name <- names(output)[cont + 1]
            Index <- which(Units == IDQual)
            output[[Name]][Index] <- Obj[['Value']][i]}
        
        }
        return(output)
    }
    
)
