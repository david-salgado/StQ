#' @title \code{\link{data.frame}} with the content of the variable names
#' correspondence
#'
#' @description \code{XLS} is a \code{\link{data.frame}} with the correspondence
#'  among the names of the variables used by conductors of different production 
#'  phases of a statistical operation.
#'
#' It contains the correspondence among the variable names of the variables
#' from the Excel file of the microdata store (see vignettes).
#'
#' The columns of the \code{\link{data.frame}} are:
#' \itemize{
#'   \item CalificadoresID: Statistical unit qualifiers.
#'   \item CalificadoresNoID: Variable names qualifiers.
#'   \item Variables: Variable names.
#'   \item NOrden: Statitistical unit qualifier name.
#'   \item CCAA: Name of the first variable name qualifier (values from
#'   \code{01} to \code{17})
#'   \item EsRemuner: Name of the second variable name qualifier (\code{1} if
#'   'Remunerado' or \code{0} if 'No remunerado').
#'   \item TipoRem: Name of the third variable name qualifier (\code{1} 'Fijo',
#'   \code{2} 'Eventual', \code{3} 'ETT', \code{4} 'Otros').
#'   \item SP: Variable names used by the conductor unit (\emph{S}ervicio
#'   \emph{P}romotor in Spanish).
#'   \item SGMRD: Variable names used by the data collection unit (\emph{S.G.}
#'   \emph{M}uestreo y \emph{R}ecogida de \emph{D}atos).
#' 
