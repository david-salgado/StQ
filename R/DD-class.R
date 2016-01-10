#' @title S4 class for the information contained in DD files
#'
#' @description Definition of an S4 class named \code{DD} with the information
#' contained in a DD file with s slightly different structure to that of those
#' files.
#'
#' The class \code{DD} comprises a slot of class \linkS4class{data.table} with
#' at least columns named \code{Variable}, \code{Sort}, \code{Class} and
#' \code{Qual1}. These columns have the same meanings:
#'
#' \itemize{
#'  \item \code{Variable}: Name of the variable.
#'  \item \code{Sort}: Semantic sort of the variable, which can be a statistical
#'  unit qualifier (\code{IDQual}), a variable (non-unit) qualifier
#'  (\code{NonIDQual}) and a variable name (\code{IDDD}).
#'  \item \code{Class}: Class of the variable (\code{integer}, \code{numeric},
#'  \code{character},...).
#'  \item \code{Qual1}: Name of the variable qualifier 1.
#' }
#'
#' This \linkS4class{data.table} is complete with as many columns named
#' \code{Qualn} as necessary.
#'
#' @slot Data \linkS4class{data.table} with at least the columns \code{Variable},
#'  \code{Sort}, \code{Class}, \code{Qual1} (in that order).
#'
#' @examples
#' # An empty DD object is built through the code:
#' new(Class = 'DD')
#'
#' # An elementary example with three variables (1 unit qualifier, 1 non-unit
#' # qualifier and 1 variable)
#' library(data.table)
#' DDData <- data.table(Variable = c('NOrden', 'CCAA', 'CifraNeg'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = c('character', 'character', 'numeric'),
#'                      Qual1 = c('', '', 'NOrden'))
#' new(Class = 'DD', Data = DDData)
#'
#' @include ExtractNames.R
#'
#' @import data.table
#'
#' @export
setClass(Class = "DD",
         slots = c(Data = 'data.table'),
         prototype = list(Data = data.table::data.table(Variable = character(0),
                                                        Sort = character(0),
                                                        Class = character(0),
                                                        Qual1 = character(0))),
         validity = function(object){

             ColNames <- names(object@Data)

             if (ColNames[1] != 'Variable') {

                 stop('[Validity DD] The first column of slot Data must be named
                      "Variable".')
             }
             if (any(duplicated(object@Data[['Variable']]))) {

                 stop('[Validity DD] The column "Variable" cannot have repeated
                      values.')
             }
             set2keyv(object@Data, 'Variable')
             if (ColNames[2] != 'Sort') {

                 stop('[Validity DD] The second column of slot Data must be
                      named "Sort".')
             }
             if (length(object@Data[['Sort']]) != 0 &&
                 !all(object@Data[['Sort']] %in%
                 c('IDQual', 'NonIDQual', 'IDDD'))) {

                 stop('[Validity DD] The column "Sort" can only have values
                      "IDQual", "NonIDQual" and "IDDD".')

             }
             if (ColNames[3] != 'Class') {

                 stop('[Validity DD] The third column of slot Data must be
                      "Class".')
             }
             if (ColNames[4] != 'Qual1') {

                 stop('[Validity DD] The fourth column of slot DD must be named
                      "Qual1".')
             }

             if (!all(object@Data[['Variable']] ==
                      ExtractNames(object@Data[['Variable']]))) {

                 stop('[Validity DD] There are invalid variable names in the
                      column "Variable".')

             }


             Quals <- setdiff(ColNames, c('Variable', 'Sort', 'Class'))
             if (!all(Quals == paste0('Qual', seq(along = Quals)))) {

                 stop('[Validity DD] The fourth and succesive columns must be
                      named "Qual1", "Qual2", ...')
             }

             return(TRUE)
         }
)
