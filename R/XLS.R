<<<<<<< HEAD
#' \code{\link{data.frame}} with the content of the variable names
#' correspondence
#'
#' \code{XLS} is a \code{\link{data.frame}} with the correspondonce among the
#' names of the variables used by conductors of different production phases of a
#' statistical operation.
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
=======
#' Hoja de Excel en formato data.frame
#'
#' \code{XLS} es un \code{\link{data.frame}} con la equivalencia entre los nombres
#' de las variables de la encuesta en la SGMRD y los nombres de las mismas 
#' en el servicio promotor.
#' 
#' Las columnas del \code{data.frame} son las siguientes:
#'\itemize{
#'   \item CalificadoresID. Calificadores identificativos de unidad.
#'   \item CalificadoresNoID. Calificadores de variable.
#'   \item Variables. Nombres de las variables en el repositorio.
#'   \item NOrden. Calificador de unidad. Aparece por completitud.
#'   \item CCAA. Comunidad autonoma (valores 01-17).
#'   \item EsRemuner. Remunerado (1) or no remunerado (0).
#'   \item TipoRem. Tipo de remuneracion (1 Fijo, 2 Eventual, 3 ETT, 4 Otros).
#'   \item SP. Nombres de las variables en el servicio promotor de la encuesta.
#'   \item SGMRD. Nombres de las variables en la SGRMD.
>>>>>>> e9efa7c0eccfd9de1f1e25ef8428b085f47e8788
#' }
#'
#' @docType data
#' @name XLS
#' @usage data(XLS)
<<<<<<< HEAD
#' @format \code{data.frame} with 68 rows and 9 columns.
NULL
=======
#' @format \code{data.frame} con 68 filas y 9 columnas.
NULL
>>>>>>> e9efa7c0eccfd9de1f1e25ef8428b085f47e8788
