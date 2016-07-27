#' @title Object of class \linkS4class{VarNameCorresp}
#'
#' @description \code{ExampleVNC} is an object of class \linkS4class{VarNameCorresp} with components
#' \code{MicroData} and \code{Aggregates}.
#'
#' The columns of component \code{MicroData} of this object are:
#' \itemize{
#'   \item \code{IDQual}: statistical unit qualifiers.
#'   \item \code{NonIDQual}: variable name qualifiers.
#'   \item \code{IDDD}: variable names.
#'   \item \code{ID}: first qualifier values.
#'   \item \code{IsRemun}: second qualifier values.
#'   \item \code{IsPartTime}: third qualifier values.
#'   \item \code{Unit}\emph{1}-\emph{3}: variable names for production units 1, 2 and 3.
#' }
#'
#' And the columns of component \code{Aggregates} are:
#' \itemize{
#'   \item \code{IDQual}: statistical unit qualifiers.
#'   \item \code{NonIDQual}: variable name qualifiers.
#'   \item \code{IDDD}: variable names.
#'   \item \code{IsRemun}: second qualifier values.
#'   \item \code{IsPartTime}: third qualifier values.
#'   \item \code{Unit}\emph{1}: variable name for production unit 1.
#' }
#' 
#' @docType data
#'
#' @name ExampleVNC
#'
#' @usage data(ExampleVNC)
#'
#' @format Object of class \code{VarNameCorresp}. Its component \code{MicroData} has 16 rows and 9
#' columns and its component \code{Aggregates} has 4 rows and 6 columns.
NULL
