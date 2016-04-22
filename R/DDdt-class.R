#' @title S4 class for \linkS4class{data.table}s as components of 
#' \linkS4class{DD} objects
#'
#' @description Definition of an S4 class named \code{DDdt} with the 
#' properties of each statistical variable.
#'
#' The class \code{DDdt} is a \linkS4class{data.table} with the following 
#' columns:
#'
#' \itemize{
#'  \item \code{Variable}: name of each variable.
#'  \item \code{Sort}: role of each variable (IDQual, NonIDQual, IDDD).
#'  \item \code{Class}: class of each variable (character, numeric, ...).
#'  \item One column of name \emph{"Qual"}\code{j} per each name of the 
#'  qualifiers needed for each variable.
#'  \item \code{ValueRegExp}: regexp for the value of each variable.
#' }
#'
#' @examples
#' new(Class = 'DDdt', 
#'     data.table(Variable = 'NOrden', 
#'                Sort = 'IDQual', 
#'                Class = 'character', 
#'                ValueRegExp = '[0-9]{9}SS'))
#' @import data.table
#'
#' @export
setClass(Class = "DDdt",
         contains = 'data.table',
         prototype = data.table(Variable = character(0),
                                Sort = character(0),
                                Class = character(0),
                                Qual1 = character(0),
                                ValueRegExp = character(0)),    
         validity = function(object){
             
             NCol <- dim(object)[2]
             NQual <- max(0, NCol - 4)
             if (NQual > 0) {
                 
                 Quals <- paste0('Qual', 1:NQual)
                 
             } else {
                 
                 Quals <- character(0)
             
             }
             ColNames <- c('Variable', 'Sort', 'Class', 'ValueRegExp', Quals)
            
             if (!all(names(object) %in% ColNames)) {
                 
                 stop('[Validity DDdt] The names of a DDdt object must be: Variable, Sort, Class, Qual1-Qualj, ValueRegExp.')
                 
             }
            
             if (!all(object[['Sort']] %in% c('IDDD', 'IDQual', 'NonIDQual'))) {
                 
                 stop('[validity DDdt] The column Sort values must be IDDD, IDQual or NonIDQual.')
             }
            
             return(TRUE)
         }
)
