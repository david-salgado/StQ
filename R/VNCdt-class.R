#' @title S4 class for \linkS4class{data.table}s as components of 
#' \linkS4class{VarNameCorresp} objects
#'
#' @description Definition of an S4 class named \code{VNCdt} with the 
#' correspondence between variable names.
#'
#' The class \code{VNCdt} is a \linkS4class{data.table} with the following 
#' columns:
#'
#' \itemize{
#'  \item \code{IDQual}: names of unit qualifiers.
#'  \item \code{NonIDQual}: names of variable qualifiers.
#'  \item \code{IDDD}: root names of the statistical variables.
#'  \item One column of name \emph{"IDQual"}\code{j} per each unit qualifier.
#'  \item One column of name \emph{"NonIDQual"}\code{k} per each variable 
#'  qualifier. 
#'  \item One column of name \emph{"Unit"}\code{p} per each production unit 
#'  which assigns their proper statistical variable names.
#' }
#'
#' @examples
#' new(Class = 'VNCdt', 
#'     data.table(IDQual = c('NOrden', '', '', ''), 
#'                NonIDQual = c('', 'IsNatMarket', '', ''), 
#'                IDDD = c('', '', 'Turnover', 'Turnover'),
#'                NOrden = c('', '', '.', '.'),
#'                IsNatMarket = c('', '', '0', '1'),
#'                Unit1 = c('', '', 'cn01', 'cn02')))
#' @import data.table
#'
#' @export
setClass(Class = "VNCdt",
         contains = 'data.table',
         prototype = data.table(IDQual = character(0),
                                NonIDQual = character(0),
                                IDDD = character(0),
                                Unit1 = character(0)),    
         validity = function(object){
             
             NCol <- dim(object)[2]
             ColNames <- names(object)
             if (ColNames[1] != 'IDQual') {
                 
                 stop('[validity VNCdt] The first column of VNCdt must be IDQual.')
             }
             if (ColNames[2] != 'NonIDQual') {
                 
                 stop('[validity VNCdt] The second column of VNCdt must be NonIDQual.')
             }
             if (ColNames[3] != 'IDDD') {
                 
                 stop('[validity VNCdt] The third column of VNCdt must be IDDD.')
             }
             
             IDQuals <- object[['IDQual']]
             IDQuals <- IDQuals[IDQuals != '']
             if (any(duplicated(IDQuals))) {
                 
                 stop('[Validity VarNameCorresp] The column "IDQual" cannot have repeated values.')
                 
             }
             IDQualCols <- intersect(ColNames, IDQuals)
             if (!all(IDQualCols == IDQuals)) {
                 
                 stop('[validity VNCdt] Every unit qualifier in column IDQual must appear also as a column in the same order.')
                 
             }
             
             NonIDQuals <- object[['NonIDQual']]
             NonIDQuals <- NonIDQuals[NonIDQuals != '']
             if (any(duplicated(NonIDQuals))) {
                 
                 stop('[Validity VarNameCorresp] The column "NonIDQual" cannot have repeated values.')
                 
             }
             NonIDQualCols <- intersect(ColNames, NonIDQuals)
             if (!all(NonIDQualCols == NonIDQuals)) {
                 
                 stop('[validity VNCdt] Every variable qualifier in column NonIDQual must appear also as a column in the same order.')
                 
             }
             
             Units <- ColNames[grep('Unit', ColNames)]
             
             ReadColNames <- c('IDQual', 'NonIDQual', 'IDDD', IDQuals, 
                               NonIDQuals, Units)
        
             if (!all(ColNames %in% ReadColNames)) {
                 
                 stop('[Validity VarNameCorresp] The names of the columns with production unit variable names must be "Unit1, Unit2, ...".')
             }
             
             return(TRUE)
         }
)
