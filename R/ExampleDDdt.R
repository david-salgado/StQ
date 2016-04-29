#' @title Object of class \linkS4class{DDdt}
#'
#' @description \code{ExampleDDdt} is an object of class \linkS4class{DDdt}.
#'
#' This class is a subclass of \linkS4class{data.table} with columns 
#' \code{Variable}, \code{Sort}, \code{Class}, \code{Qual}\emph{1}-\emph{j}, and
#' \code{ValueRegExp}.
#'
#' The meaning of these columns are:
#' \itemize{
#'   \item \code{Variable}: statistical variable name.
#'   \item \code{Sort}: semantic sort of the variable (IDQual, NonIDQual, IDDD).
#'   \item \code{Class}: class of the variable (\code{numeric}, \code{character},...).
#'   \item \code{Qual1} to \code{Qual3}: 1st to 3rd variable qualifier.
#'   \item \code{ValueRegExp}: regexp for the variable values.
#' }
#' 
#' @docType data
#'
#' @name ExampleDDdt
#'
#' @usage data(ExampleDDdt)
#'
#' @format Object of class \linkS4class{DDdt} with 14 rows and 7 columns.
NULL
