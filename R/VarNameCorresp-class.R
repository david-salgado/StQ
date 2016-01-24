#' @title S4 class for the variable names correspondence
#'
#' @description Definition of an S4 class named \code{VarNameCorresp} with the 
#' correspondence between variable names used by different production units for
#' a given statistical variable.
#'
#' The class \code{VarNameCorresp} comprises a slot of class \linkS4class{list}
#' whose components are \code{data.table} with a row per each variable and the
#' following columns:
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
#' VarList <- list(MicroData = data.table(IDQual = c('NumIdEst', '', '', '', 
#'                                                   '', ''),
#'                                        NonIDQual = c('', 'EsMercNac', 
#'                                                      'EsMercEuro', 
#'                                                      'EsMercRM',
#'                                                      'Cod',''),
#'                                        IDDD = c('', '', '', '', '', 
#'                                                 'IEPEntradaPed'),
#'                                        NumIdEst = c('', '', '', '', '', ''),
#'                                        EsMercNac = c('', '', '', '', '', '0'),
#'                                        EsMercEuro = c('', '', '', '', '', '0'),
#'                                        EsMercRM = c('', '', '', '', '', '1'),
#'                                        Cod = c('', '', '', '', '', ''),
#'                                        Unit1 = c('', '', '', '', '', 'cp09')))
#' new(Class = 'VarNameCorresp', VarNameCorresp = VarList)
#'
#' @import data.table
#'
#' @export
setClass(Class = "VarNameCorresp",
         slots = c(VarNameCorresp = 'list'),
         prototype = list(VarNameCorresp = list(data.table(IDQual = character(0),
                                                      NonIDQual = character(0),
                                                      IDDD = character(0),
                                                      Unit1 = character(0)))),
         validity = function(object){
         
         if (is.null(names(object@VarNameCorresp))) stop('[Validity VarNameCorresp] VarNameCorresp slot must be a named list.')
         SlotClasses <- unlist(lapply(object@VarNameCorresp, 
                                      function(x){class(x)[1]}))
         if (!all(SlotClasses == 'data.table')) stop('[Validity VarNameCorresp] All components of slot VarNameCorresp must be data.tables.')     
         
         VNCCompNames <- as.list(names(object@VarNameCorresp))   
         lapply(VNCCompNames, function(VNCCompName){
             
                ColNames <- names(object@VarNameCorresp[[VNCCompName]])
                if (ColNames[1] != 'IDQual'){
                
                 stop(paste0('[Validity VarNameCorresp] The first column of data.table ', VNCCompName, ' must be named "IDQual".'))
                }
              IDQual <- object@VarNameCorresp[[VNCCompName]][['IDQual']]
              IDQual <- sort(IDQual[IDQual != ""])
              if (any(duplicated(IDQual))){
                
                stop('[Validity VarNameCorresp] The column "IDQual" cannot have repeated values.')
              }
              NonIDQual <- object@VarNameCorresp[[VNCCompName]][['NonIDQual']]
              NonIDQual <- sort(NonIDQual[NonIDQual!=""])
              
              if (length(NonIDQual) > 0 && ColNames[2] != 'NonIDQual'){
                
                stop(paste0('[Validity VarNameCorresp] The second column of data.table ', VNCCompName, ' must be named "NonIDQual".'))
              }
              
              if (any(duplicated(NonIDQual))) {
                
                stop('[Validity VarNameCorresp] The column "NonIDQual" cannot have repeated values.')
              }
              
              #if (length(NonIDQual) == 0 && ColNames[2] != 'IDDD'){
                
              #  stop(paste0('[Validity VarNameCorresp] The second column of data.table ', VNCCompName, ' must be named "IDDD".'))
              #}
              
              #if (length(NonIDQual) > 0 && ColNames[3] != 'IDDD'){
                  
              #    stop(paste0('[Validity VarNameCorresp] The third column of data.table ', VNCCompName, ' must be named "IDDD".'))
              #}     
              
              ColIDQual <- sort(ColNames[4:(3 + length(IDQual))])
              if (length(ColNames) > 4){
                
                 if (length(ColIDQual[ColIDQual != IDQual]) > 0){
                   
                   stop(paste0('[Validity VarNameCorresp] The names of unit qualifiers in the columns of data.table ', VNCCompName, ' are not right. '))
                 }
                  
                 if (length(NonIDQual > 0)){
                   ColNonIDQual <- sort(ColNames[(4 + length(IDQual)):((3 + length(IDQual)) + length(NonIDQual))])
                   #print(paste0('NonIDQual: ', NonIDQual))
                   #print(paste0('ColNonIDQual: ', ColNonIDQual))     
                   if (length(ColNonIDQual[ColNonIDQual != NonIDQual]) > 0){
                     
                     stop(paste0('[Validity VarNameCorresp] The names of the (non-unit) qualifier in the columns of data.table ', VNCCompName, ' are not right. '))
                   }
                 }
                
                Unitn <- ColNames[grep('Unit', ColNames)]
    
                if (length(Unitn[Unitn != paste0('Unit', seq(along = Unitn))]) > 0){
                  
                  stop('[Validity VarNameCorresp] The names of the columns with Units must be "Unit1, Unit2, ...".')
                } 
                 
              } else {
              
                if (ColNames[4] != 'Unit1'){
                  
                  stop(paste0('[Validity VarNameCorresp] The fourth column of data.table ', VNCCompName, ' must be named "Unit1".'))
                }
              }
              if (length(NonIDQual) > 0) {
                  setorderv(object@VarNameCorresp[[VNCCompName]], 
                            c(c('IDQual', 'NonIDQual'), 
                              setdiff(names(object@VarNameCorresp[[VNCCompName]]), 
                                      c('IDQual', 'NonIDQual'))), 
                            order = -1)
              } else {
                  setorderv(object@VarNameCorresp[[VNCCompName]], 
                            c(c('IDQual'), 
                              setdiff(names(object@VarNameCorresp[[VNCCompName]]), 
                                      c('IDQual'))), 
                            order = -1)
                  
              }
          })
          
          return(TRUE)
         }
)
