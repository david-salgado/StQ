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
#' library(data.table)
#' new(Class = 'DDdt', 
#'     data.table(Variable = 'NOrden', 
#'                Sort = 'IDQual', 
#'                Class = 'character',
#'                QualOrder = '1', 
#'                Qual1 = 'NOrden', 
#'                ValueRegExp = '[0-9]{9}SS'))
#' @import data.table
#'
#' @export
setClass(Class = "DDdt",
         contains = c('data.table'),
         prototype = data.table(Variable = character(0),
                                Sort = character(0),
                                Class = character(0),
                                QualOrder = character(0),
                                Qual1 = character(0),
                                ValueRegExp = character(0)),    
         validity = function(object){
             
             NCol <- dim(object)[2]
             
             if (NCol < 6) {
                 stop(paste0('[Validity DDdt] The object must be a data.table with 
                                at least five columns named "Variable", "Sort", "Class", 
                                "QualOrder", "Qual1" and "ValueRegExp"'))   
             } 
             
             
             Data <- slot(object, ".Data")
             if (!all(unlist(lapply(Data, function(x){is.character(unlist(x)) }))) == TRUE) {
                 
                 stop(paste0('[Validity DDdt] All columns of the object must be character vectors.'))
                 
             }
             
             QualOrder <- object[['QualOrder']]
             QualOrder <- QualOrder[QualOrder != '']
             if (length(QualOrder) != length(unique(QualOrder))) {
                 
                 stop('[Validity DDdt] Orders of qualifiers repeated in the column QualOrder.')
             }
             
             NQual <- max(0, NCol - 6)
             if (NQual > 0) {
                 
                 Quals <- paste0('Qual', 1:(NQual + 1))
                 
             } else {
                 
                 Quals <- 'Qual1'
             
             }
             ColNames <- c('Variable', 'Sort', 'Class', 'QualOrder', 'ValueRegExp', Quals)
            
             if (!all(names(object) %in% ColNames)) {
                 
                 stop('[Validity DDdt] The names of a DDdt object must be: Variable, Sort, "Class, Qual1-Qualj, ValueRegExp.')
                 
             }
             
             ColNames <- slot(object, "names")
             if (ColNames[1] != 'Variable') {
                 stop(paste0('[Validity DDdt] The first column object must be "Variable".'))   
             }
             
             if (any(duplicated(object[['Variable']]))) {
                 stop(paste0('[Validity DDdt] The column "Variable" of the object cannot have repeated values.'))
             }
             
             if (ColNames[2] != 'Sort') {
                 stop(paste0('[Validity DDdt] The second column of object must be "Sort".'))
             }
             
             if (length(object[['Sort']]) != 0 &&  !all(object[['Sort']] %in% c('IDQual', 'NonIDQual', 'IDDD'))) { 
                 stop(paste0('[Validity DDdt] The column "Sort" of the object can only have values "IDQual", "NonIDQual" and "IDDD".'))
             }
             
             if (ColNames[3] != 'Class') {
                 stop(paste0('[Validity DDdt] The third column of the object must be named "Class".'))
             }
             
             if (ColNames[4] != 'QualOrder') {
                 stop(paste0('[Validity DDdt] The fourth column of the object must be named "QualOrder".'))
                 
             }
             
             if (ColNames[5] != 'Qual1') {
                 stop(paste0('[Validity DDdt] The fifth column of of the object must be named "Qual1".'))
             }
             
             
             if (length(Quals) == 1 ) {
                 
                if (ColNames[6] != 'ValueRegExp') {
                    stop(paste0('[Validity DDdt] The sixth column of of the object must be named "ValueRegExp".'))
                }
            }
             
            return(TRUE)
         }
)
