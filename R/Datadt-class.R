#' @title S4 class for \linkS4class{data.table}s as components of 
#' \linkS4class{StQ} objects
#'
#' @description Definition of an S4 class named \code{Datadt} as a subclass of 
#' class \linkS4class{data.table}.
#'
#' The class \code{Datadt} is a \linkS4class{data.table} with the following 
#' columns:
#'
#' \itemize{
#'  \item One column of name \emph{"IDQual"}\code{j} per each unit qualifier.
#'  \item One column of name \emph{"NonIDQual"}\code{k} per each variable 
#'  qualifier.
#'  \item \code{IDDD}: root names of the statistical variables.
#'  \item \code{Value}: values of the each statistical variable.
#' }
#'
#' @examples
#' library(data.table)
#' new(Class = 'Datadt', 
#'     data.table(ID = c('001', '001', '001', '001', 
#'                       '002', '002', '002', '002'), 
#'                IsNatMarket = c('0', '1', '', '',
#'                                '0', '1', '', ''),
#'                IDDD = c('Turnover', 'Turnover', 'Province', 'NACE09',
#'                         'Turnover', 'Turnover', 'Province', 'NACE09'),
#'                Value = c('625000', '23154', '04', '0512',
#'                          '25345', '1224', '01', '0601')))
#' 
#' # A more realistic example:
#' ExampleDatadt                         
#'                          
#' @import data.table
#'
#' @export
setClass(Class = "Datadt",
         contains = 'data.table',
         prototype = prototype(data.table(IDDD = character(0),
                                          Value = character(0))),    
         validity = function(object){
             
             NCol <- dim(object)[2]
             ColNames <- names(object)
             if (ColNames[NCol] != 'Value') {
                 
                 stop('[validity Datadt] The last column of Datadt must be Value.')
             }
             if (ColNames[NCol - 1] != 'IDDD') {
                 
                 stop('[validity Datadt] The last second column of Datadt must be IDDD.')
             }

             return(TRUE)
         }
)
