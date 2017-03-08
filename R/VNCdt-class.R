#' @title S4 class for \linkS4class{data.table}s as components of \linkS4class{VarNameCorresp} 
#' objects
#'
#' @description Definition of an S4 class named \code{VNCdt} with the correspondence between 
#' variable names.
#'
#' The class \code{VNCdt} is a \linkS4class{data.table} with the following columns:
#'
#' \itemize{
#'  \item \code{IDQual}: names of unit qualifiers.
#'  \item \code{NonIDQual}: names of variable qualifiers.
#'  \item \code{IDDD}: root names of the statistical variables.
#'  \item One column of name \emph{"IDQual"}\code{j} per each unit qualifier.
#'  \item One column of name \emph{"NonIDQual"}\code{k} per each variable
#'  qualifier.
#'  \item \code{UnitName}: statistical variable names currently used in production.
#'  \item \code{InFiles}: codes for the files where each variable is to be included.
#' }
#'
#' @examples
#' library(data.table)
#' new(Class = 'VNCdt',
#'     data.table(IDQual = c('NOrden', '', '', ''),
#'                NonIDQual = c('', 'IsNatMarket', '', ''),
#'                IDDD = c('', '', 'Turnover', 'Turnover'),
#'                NOrden = c('', '', '.', '.'),
#'                IsNatMarket = c('', '', '0', '1'),
#'                UnitName = c('', '', 'cn01', 'cn02'),
#'                InFiles = rep('FF', 4)))
#'                
#' @import data.table
#'
#' @export
setClass(Class = "VNCdt",
         contains = c('data.table'),
         prototype = prototype(data.table(IDQual = character(0),
                                          NonIDQual = character(0),
                                          IDDD = character(0),
                                          UnitName = character(0),
                                          InFiles = character(0))),
         validity = function(object){

             NCol <- dim(object)[2]
             ColNames <- names(object)

             if (ColNames[1] != 'IDQual') {

                 stop('[StQ::validity VNCdt] The first column of VNCdt must be "IDQual".')
             }
             if (ColNames[2] != 'NonIDQual') {

                 stop('[StQ::validity VNCdt] The second column of VNCdt must be "NonIDQual".')
             }
             if (ColNames[3] != 'IDDD') {

                 stop('[StQ::validity VNCdt] The third column of VNCdt must be "IDDD".')
             }

             IDQuals <- object[['IDQual']]
             IDQuals <- IDQuals[IDQuals != '']
             if (any(duplicated(IDQuals))) {

                 stop('[StQ::Validity VNCdt] The column "IDQual" cannot have repeated values.')

             }
             IDQualCols <- intersect(ColNames, IDQuals)
             if (!all(IDQuals %in% IDQualCols)) {

                 stop('[StQ::validity VNCdt] Every unit qualifier in column IDQual must appear also as a column in the same order.')

             }

             NonIDQuals <- object[['NonIDQual']]
             NonIDQuals <- NonIDQuals[NonIDQuals != '']
             if (any(duplicated(NonIDQuals))) {

                 stop('[StQ::Validity VNCdt] The column "NonIDQual" cannot have repeated values.')

             }
             NonIDQualCols <- intersect(ColNames, NonIDQuals)
             if (!all(NonIDQuals %in% NonIDQualCols)) {

                 stop('[StQ::validity VNCdt] Every variable qualifier in column NonIDQual must appear also as a column in the same order.')

             }

             if (ColNames[length(ColNames) - 1] != 'UnitName') {
                 
                 stop('[StQ::validity VNCdt] The penultimate column of VNCdt must be "UnitName".')
             }
             
             if (ColNames[length(ColNames)] != 'InFiles') {
                 
                 stop('[StQ::validity VNCdt] The last column of VNCdt must be "InFiles".')
             }
             
             #Units <- ColNames[grep('Unit', ColNames)]

             #UnitColNames <- setdiff(ColNames, c('IDQual', 'NonIDQual', 'IDDD', IDQuals, NonIDQuals, 'InFiles'))
         #if (!all(UnitColNames %in% Units)) {

             #stop('[StQ::validity VNCdt] The name of the column with production unit variable names must be "UnitName".')
             #}

             return(TRUE)
         }
)
