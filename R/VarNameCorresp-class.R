#' @title S4 class for the variable names correspondence
#'
#' @description Definition of an S4 class named \code{VarNameCorresp} with the 
#' correspondence between variable names used by different production units for
#' a given statistical variable.
#'
#' The class \code{VarNames} comprises a slot of class \linkS4class{list} whose
#' components are data.tables with a row per each variable and the following 
#' columns:
#'
#' \itemize{
#'  \item \code{IDQual}: Names of unit qualifiers.
#'  \item \code{NonIDQual}: Names of variable names qualifiers.
#'  \item \code{IDDD}: Names of the variables.
#'  \item \emph{One column per each name of the \code{IDQual} qualifiers}: 
#'  Values of the corresponding \code{IDQual} Qualifier for each variable (row).
#'  \item \emph{One column per each name of the \code{NonIDQual} qualifiers}: 
#'  Values of the corresponding \code{NonIDQual} Qualifier for each variable 
#'  (row).
#'  \item \code{ProdUnit}\emph{j}: Variable name used by production Unit\emph{j}.
#' }
#'
#' @slot VarNameCorresp \linkS4class{list} with as many data.tables as tables of
#' variable names correspondences.
#'
#' @examples
#' library(data.table)
#' VarList <- list(data.table(IDQual = c('NumIdEst','','','','',''),
#'                      NonIDQual = c('', 'EsMercNac', 'EsMercEuro', 'EsMercRM',
#'                                    'Cod',''),
#'                      IDDD = c('','','','','', 'IEPEntradaPed'),
#'                      Unit1 = c('','','','','','')))
#' new(Class = 'VarNameCorresp', VarNameCorresp = VarList)
#'
#' @import data.table
#'
#' @export
setClass(Class = "VarNameCorresp",
         slots = c(VarNameCorresp = 'list'),
         prototype = list(VarNameCorresp = data.table(IDQual = character(0),
                                                      NonIDQual = character(0),
                                                      IDDD = character(0),
                                                      Unit1 = character(0))),
         validity = function(object){
        
         if (is.null(names(object@VarNameCorresp))) {
           
           names(object@VarNameCorresp) <- seq(along = object@VarNameCorresp)  
         }
              
         lapply(object@VarNameCorresp, function(SheetName){  
                ColNames <- names(SheetName)
                if (ColNames[1] != 'IDQual'){
                
                 stop('[Validity VarNameCorresp] The first column of slot VarNames must be named "IDQual".')
                }
              IDQual <- SheetName[['IDQual']]
              IDQual <- IDQual[IDQual != ""]
              if (any(duplicated(IDQual))){
                
                stop('[Validity VarNames] The column "IDQual" cannot have repeated values.')
              }
              
              if (ColNames[2] != 'NonIDQual'){
                
                stop('[Validity VarNames] The second column of slot VarNames must be named "NonIDQual".')
              }
              NonIDQual <- SheetName[['NonIDQual']]
              NonIDQual <- NonIDQual[NonIDQual!=""]
              if (any(duplicated(NonIDQual))) {
                
                stop('[Validity VarNames] The column "NonIDQual" cannot have repeated values.')
              }
              
              if (ColNames[3] != 'IDDD'){
                
                stop('[Validity VarNames] The third column of slot VarNames must be named "IDDD".')
              }
    
              ColIDQual <- ColNames[4:(3+length(IDQual))]
              if (length(ColNames) > 4){
                
                 if (length(ColIDQual[ColIDQual != IDQual]) > 0){
                   
                   stop('[Validity VarNames] The names of unit qualifiers in the columns of slot VarNames are not right. ')
                 }
                  
                 if (length(NonIDQual > 0)){
                   
                   ColNonIDQual <- ColNames[(4+length(IDQual)):((3+length(IDQual))+length(NonIDQual))]
                   if (length(ColNonIDQual[ColNonIDQual != NonIDQual]) > 0){
                     
                     stop('[Validity VarNames] The names of the (non-unit) qualifier in the columns of slot VarNames are not right. ')
                   }
                 }
                
                Unitn <- ColNames[(4+length(IDQual)+length(NonIDQual)):length(ColNames)]
    
                if (length(Unitn[Unitn != paste0('Unit',seq(along = Unitn))]) > 0){
                  
                  stop('[Validity VarNames] The names of the columns with Units must be "Unit1, Unit2, ...".')
                } 
                 
              }else{
              
                if (ColNames[4] != 'Unit1'){
                  
                  stop('[Validity VarNames] The fourth column of slot VarNames must be named "Unit1".')
                }
              }
          })
          
          return(TRUE)
         }
)
