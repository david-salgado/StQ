#' @title Extract the root name of compound variable names
#'
#' @description \code{ExtractNames} returns a \code{character} vector with the root names of the 
#' input parameter.
#'
#' This function is thought to be applied on those variable names with suffixes containing 
#' underscores (_) and qualifier values. It takes as input parameter \code{NamesVector} a character 
#' vector with the names of possibly several variables and extracts their root names dropping out 
#' every suffix. The returned variable root names must have an exact correspondence with the 
#' variable names (column "Variable") of class \linkS4class{DD}.
#'
#' @param NamesVector \code{Character} vector with the original compound variable names.
#'
#' @return \code{Character} vector of the same length as the input parameter \code{NamesVector} with
#'  the corresponding root names of each variable name.
#'
#' @examples
#' ExtractNames(c('ICMCifraNeg', 'ICMEmpleo_1.', 'ICMEmpleo_2.1', 'ICMEmpleo_2.2'))
#' ExtractNames(c('IASSCifraNeg.x', 'IASSCifraNeg.y'))
#' ExtractNames(c('ICMCifraNeg.x', 'ICMEmpleo_1..x', 'ICMEmpleo_2.1.x', 'ICMEmpleo_2.2.x',
#'                'ICMCifraNeg.y', 'ICMEmpleo_1..y', 'ICMEmpleo_2.1.y', 'ICMEmpleo_2.2.y'))
#'                
#' @export
ExtractNames <- function(NamesVector){

    NamesVector.list <- as.list(NamesVector)
    output1 <- unlist(lapply(NamesVector.list,
                             function(name){strsplit(name,
                                                     '.',
                                                     fixed = TRUE)[[1]][1]}))
    output2 <- unlist(lapply(as.list(output1),
                             function(name){strsplit(name, '_')[[1]][1]}))
    return(output2)

}
