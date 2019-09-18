setClassUnion('characterOrcall', c('character', 'call'))
#' S4 class with the main attributes of a variable
#'
#' @description Definition of an S4 class named \code{Variable} with its main attributes
#' 
#' @slot UnitName \code{character} vector of lenght 1 with the UnitName of the new variable to 
#' create.
#'
#' @slot IDDD \code{character} vector of length 1 with the IDDD name of the new variable to create.
#' 
#' @slot QualsValues Named list with the names of the qualifiers of the new variable and their
#' respective values.
#' 
#' @slot Length \code{character} vector of length 1 with the maximum length for the values of the
#' variable. 
#' 
#' @slot ClassVar \code{character} vector of length 1 with the class of the variable (numeric o 
#' character)
#' 
#' @slot ValueRegExp \code{character} vector of length 1 with the expression of right values for the
#' variable.
#' 
#' @slot Formula  with the character expression to calculate the new variable.
#' 
#' @slot SlotName \code{character} vector of length 1 with the name of the DD slot in which the
#' new variable will be created.
#' 
#' @slot Literal \code{character} vector of length 1 with a comment for the variable.
#' 
#' @examples
#' Var <- new(Class = 'Variable', UnitName = 'LRTEmp',
#'                                IDDD = 'Employees',
#'                                QualsValues = list(ID = '', EmplType = ''),
#'                                Length = '8',
#'                                ClassVar = 'numeric',
#'                                ValueRegExp = '[.]+',
#'                                Formula = as.call(list('log( 1 + (Turnover / (Employees_1. + Employees_2.1.)))')),
#'                                SlotName = 'MicroData',
#'                                Literal = '',
#'                                DDversion = '1')
#'
#' @export
setClass(
  Class = 'Variable',
  slots = c(UnitName = 'character',
            IDDD = 'character',
            QualsValues = 'list',
            Length = 'character',
            ClassVar = 'character',
            ValueRegExp = 'character',
            Formula = 'characterOrcall',
            SlotName = 'character',
            Literal = 'character',
            DDversion = 'character'
  ),
  validity = function(object){
    
    if (length(object@UnitName) != 1) stop('[Variable validation] UnitName must be a character vector of length 1.')
    if (length(object@IDDD) != 1) stop('[Variable validation] IDDD must be a character vector of length 1.')
    if (length(object@Length) != 1) stop('[Variable validation] Length must be a character vector of length 1.')
    if (length(object@ClassVar) != 1) stop('[Variable validation] ClassVar must be a character vector of length 1.')
    if (length(object@ValueRegExp) != 1) stop('[Variable validation] ValueRegExp must be a character vector of length 1.')
    if (length(object@Literal) != 1) stop('[Variable validation] Literal must be a character vector of length 1.')
    if (length(object@DDVersion) != 1) stop('[Variable validation] DDVersion must be a character vector of length 1.')
    if (!object@SlotName %in% c('ID', 'MicroData', 'ParaData', 'Aggregates', 'AggWeights', 'Other' )) stop('[Variable validation] The valid names for SlotName parameter are: ID, MicroData, ParaData, Aggregates, AggWeights or Other.')
    if (!object@ClassVar %in% c('character', 'numeric')) stop('[Variable validation] The valid names for ClassVar parameter are character and numeric.')
    if (length(names(object@QualsValues)) != length(object@QualsValues))  stop('[Variable validation] All elements in QualsValues parameter must have a name.')
    
    return(TRUE)
  }
)
