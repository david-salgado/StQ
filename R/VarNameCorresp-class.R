#' @title S4 class for the correspondence between variable names
#'
#' @description Definition of an S4 class named \linkS4class{VarNameCorresp} with the correspondence 
#' between variable names used by the different production unit for a given statistical variable and
#' variable names according to the key-value pair data model with qualifiers given by statistical 
#' metadata.
#'
#' The class \linkS4class{VarNameCorresp} comprises a \linkS4class{list} whose components are 
#' \linkS4class{data.table}s with a row per each variable and the following columns:
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
#'  \item \code{UnitName}: Variable name used in production.
#'  \item \code{InFiles}: code of the files where each variable is included.
#' }
#'
#' @examples
#' library(data.table)
#' VarList <- list(ID = new(Class = 'VNCdt',
#'                 data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                            NonIDQual = c('','','','',''),
#'                            IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                            NumIdEst = c('', rep('.', 4)),
#'                            UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                            InFiles = rep('FI', 5))),
#' MicroData =new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                            NonIDQual = c('', 'Market', ''),
#'                                            IDDD = c(rep('', 2), 'NewOrders'),
#'                                            NumIdEst = c(rep('', 2), '.'),
#'                                            Market = c(rep('', 2), '1'),
#'                                            UnitName = c('numidest', '', 'cp09'),
#'                                            InFiles = rep('FF, FD, FG', 3))),
#' ParaData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                            NonIDQual = c('', 'Action', ''),
#'                                            IDDD = c(rep('', 2), 'Date'),
#'                                            NumIdEst = c(rep('', 2), '.'),
#'                                            Action = c(rep('', 2), 'Imputation'),
#'                                            UnitName = c('numidest', '', 'FechaImput'),
#'                                            InFiles = rep('FP', 3))))
#'
#' VNC <- new(Class = 'VarNameCorresp', VarList)
#' VNC
#'
#' @import data.table 
#' 
#' @include VNCdt-class.R
#'
#' @export
setClass(Class = "VarNameCorresp",
         contains = 'list',
         prototype = prototype(list(ID = new(Class = 'VNCdt'),
                                    MicroData = new(Class = 'VNCdt'),
                                    ParaData = new(Class = 'VNCdt'))),    
         
         validity = function(object){
         
         if (is.null(names(object))) {
             
             stop('[StQ:: Validity VarNameCorresp] A VarNameCorresp object must be a named list.')
         
         }
         
         ComponentNames <- sort(names(object))
         RootCompNames <- unlist(lapply(ComponentNames, 
                                        function(Name){
                                            strsplit(Name, '_', fixed = TRUE)[[1]][1]
         }))
         RootCompNames <- unique(RootCompNames)

         if (!all(RootCompNames %in% c('ID',
                                       'MicroData',
                                       'ParaData',
                                       'Aggregates',
                                       'AggWeights',
                                       'Other'))) {
             
             stop('[StQ::validity VarNameCorresp] The valid names must be any of ID, MicroData, ParaData, Aggregates, AggWeights, Other or be compound with underscores beginning with these names.\n')
             
         }
                  
         if(!all(c('ID', 'MicroData', 'ParaData') %in% RootCompNames)) {
                 
                 stop('[StQ::Validity VarNameCorresp] A VarNamesCorresp object must be a named list with at least components with names ID, MicroData and Paradata or compound names with underscores beginning with these names.\n')
                 
         }     
             
         ComponentClasses <- unlist(lapply(object, function(x){class(x)[1]}))
         if (!all(ComponentClasses == 'VNCdt')) {
             
             stop('[StQ::Validity VarNameCorresp] All components of slot VarNameCorresp must be objects of class VNCdt.\n')     
         
         }
          
          return(TRUE)
         }
)
