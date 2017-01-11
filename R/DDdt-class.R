#' @title S4 class for \linkS4class{data.table}s with columns containing the variable specifications
#'
#' @description Definition of an S4 class named \code{DDdt} with the properties of each statistical 
#' variable. This is a subclass of \linkS4class{data.table} used as slots in a \linkS4class{DD}
#' object.
#'
#' The class \code{DDdt} is a \linkS4class{data.table} with the following columns (in this order):
#'
#' \itemize{
#'  \item \code{Variable}: name of each variable.
#'  \item \code{Sort}: role of each variable (IDQual, NonIDQual, IDDD).
#'  \item \code{Class}: class of each variable (character, numeric, ...).
#'  \item \code{Length}: highest length for each variable (in terms of number of characters).
#'  \item One column of name [\emph{Qual}\code{j}] per each name of the qualifiers needed for each 
#'  variable.
#'  \item \code{ValueRegExp}: regexp for the value of each variable (not active yet).
#' }
#'
#' @examples
#' library(data.table)
#' new(Class = 'DDdt', 
#'     data.table(Variable = 'NOrden', 
#'                Sort = 'IDQual', 
#'                Class = 'character',
#'                Length = '11', 
#'                Qual1 = 'NOrden', 
#'                ValueRegExp = '[0-9]{9}SS'))
#'                
#' data(ExampleDDdt)
#' ExampleDDdt
#' 
#' @import data.table
#'
#' @export
setClass(Class = "DDdt",
         contains = c('data.table'),
         prototype = prototype(data.table(Variable = character(0),
                                          Sort = character(0),
                                          Class = character(0),
                                          Length = character(0),
                                          Qual1 = character(0),
                                          ValueRegExp = character(0))),    
         validity = function(object){
             
             NCol <- dim(object)[2]
             
             if (NCol < 6) {
                 
                 stop(paste0('[StQ:: Validity DDdt] The object must be a data.table with at least five columns named "Variable", "Sort", "Class", "Length", "Qual1" and "ValueRegExp".\n'))   
            } 
             
             
             Data <- slot(object, ".Data")
             if (!all(unlist(lapply(Data, function(x){is.character(unlist(x)) }))) == TRUE) {
                 
                 stop(paste0('[StQ:: Validity DDdt] All columns of the object must be character vectors.\n'))
                 
             }

             setkeyv(object, 'Variable')
             if (sum(duplicated(object, by = key(object))) > 0) {
                 
                 stop('[StQ:: Validity DDdt] No duplicate variable is allowed.\n')
                 
             }
             
             NQual <- max(0, NCol - 6)
             if (NQual > 0) {
                 
                 Quals <- paste0('Qual', 1:(NQual + 1))
                 
             } else {
                 
                 Quals <- 'Qual1'
             
             }
             ColNames <- c('Variable', 'Sort', 'Class', 'Length', 'ValueRegExp', Quals)
            
             if (!all(names(object) %in% ColNames)) {
                 
                 stop('[StQ:: Validity DDdt] The names of a DDdt object must be: Variable, Sort, Class, Length, Qual1-Qualj, ValueRegExp.\n')
                 
             }
             
             ColNames <- slot(object, "names")
             if (ColNames[1] != 'Variable') {
                 
                 stop(paste0('[StQ:: Validity DDdt] The first column object must be "Variable".\n'))   
             }
             
             if (any(duplicated(object[['Variable']]))) {
                 
                 stop(paste0('[StQ:: Validity DDdt] The column "Variable" of the object cannot have repeated values.\n'))
             }
             
             if (ColNames[2] != 'Sort') {
                 
                 stop(paste0('[StQ:: Validity DDdt] The second column of object must be "Sort".\n'))
             }
             
             if (length(object[['Sort']]) != 0 &&  !all(object[['Sort']] %in% c('IDQual', 'NonIDQual', 'IDDD'))) { 
                 
                 stop(paste0('[StQ:: Validity DDdt] The column "Sort" of the object can only have values "IDQual", "NonIDQual" and "IDDD".\n'))
             }
             
             if (ColNames[3] != 'Class') {
                 
                 stop(paste0('[StQ:: Validity DDdt] The third column of the object must be named "Class".\n'))
             }
             
             if (ColNames[4] != 'Length') {
                 
                 stop(paste0('[StQ:: Validity DDdt] The fourth column of the object must be named "Length".\n'))
                 
             }
             
             if (unique(substr(ColNames[5:(4 + length(Quals))], 1, 4)) != 'Qual') {
                 
                 stop(paste0('[StQ:: Validity DDdt] The fifth and next (if any) columns of the object must be named "Qualj".\n'))
             }
             
             
             if (ColNames[length(ColNames)] != 'ValueRegExp') {
                    
                    stop(paste0('[StQ:: Validity DDdt] The last column of of the object must be named "ValueRegExp".\n'))
            }
             
            return(TRUE)
         }
)
