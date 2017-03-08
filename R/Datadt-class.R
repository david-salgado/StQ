#' @title S4 class as a subclass of \linkS4class{data.table} with columns for qualifiers and columns 
#' for variable names and variable values
#'
#' @description Definition of an S4 class named \code{Datadt} as a subclass of 
#' class \linkS4class{data.table}. This subclass \code{Datadt} is a \code{data.table} with the 
#' following columns (in this order):
#'
#' \itemize{
#'  \item One column of name [\emph{IDQual}\code{j}] per each unit qualifier.
#'  \item One column of name [\emph{NonIDQual}\code{k}] per each variable qualifier.
#'  \item \code{IDDD}: root names of each statistical variable.
#'  \item \code{Value}: values of each statistical variable.
#' }
#'
#' @examples
#' library(data.table)
#' new(Class = 'Datadt', 
#'     data.table(ID = c('001', '001', '001', '001', '002', '002', '002', '002'), 
#'                Market = c('0.', '1.', '', '', '0.', '1.', '', ''),
#'                IDDD = c('Turnover', 'Turnover', 'Province', 'NACE09',
#'                         'Turnover', 'Turnover', 'Province', 'NACE09'),
#'                Value = c('625000', '23154', '04', '0512', '25345', '1224', '01', '0601')))
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
                 
                stop('[StQ:: validity Datadt] The last column of Datadt must be Value.')
             
            }
            if (ColNames[NCol - 1] != 'IDDD') {
                 
                stop('[StQ:: validity Datadt] The last second column of Datadt must be IDDD.')
            }
            return(TRUE)
         }
)
