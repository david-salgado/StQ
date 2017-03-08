#' @title Return root names and NonID-qualifier values of compound variable names
#'
#' @description \code{VarNamesToDT} returns a \linkS4class{data.table} identifying those qualifiers
#' corresponding to the values appearing in the compound variable names specified in the input
#' parameter \code{VarNames}.
#'
#' This function is designed for variable names with suffixes appending qualifier values with
#' underscores _.
#'
#' The function determines the correspondind qualifier names for the values contained in the
#' compound variable names using the information from the \linkS4class{DD} object specified as the
#' second input parameter.
#'
#' \code{VarNamesToDD} has been designed fundamentally for internal use in the construction of
#' editing strategies, but it can also be of utility in some scripts.
#'
#' @param VarNames Character vector with the compound variable names.
#'
#' @param DD Object of class \linkS4class{DD} with the definition and properties of the variables.
#'
#' @return \linkS4class{data.table} with as many rows as the length of \code{VarNames}, with the
#' column \code{IDDD} containing the root name and one more column for each suffix in the compound
#' input name under the corresponding qualifier name. The resulting \linkS4class{data.table}
#' contains the values of each qualifier for each input variable name.
#'
#'
#' @examples
#' data(ExampleDD)
#' VarNamesToDT(c('Employees_1.'), ExampleDD)
#'
<<<<<<< HEAD
#' @include ExtractNames.R getVariables.R 
=======
#' @include ExtractNames.R getVariables.R DatadtToDT.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @import data.table
#'
#' @export
VarNamesToDT <- function(VarNames, DD){

    NotPresentVar  <- setdiff(ExtractNames(VarNames), getVariables(DD))
    if (length(NotPresentVar) > 0) stop(paste0('[StQ::VarNamesToDD] The following variables are not contained in the DD slot: ', NotPresentVar, '.\n'))
    # Para una sola variable
    if (is.character(VarNames) & length(VarNames) == 1){

<<<<<<< HEAD
        DDSlotNames <- setdiff(names(DD), 'VNC')
=======
        DDSlotNames <- setdiff(slotNames(DD), 'VarNameCorresp')
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8

        output <- list()
        for (DDslot in DDSlotNames){

<<<<<<< HEAD
            DDlocal <- DD[[DDslot]]

            Names.DT <- DDlocal[Variable == ExtractNames(VarNames)]
=======
            DDlocal <- slot(DD, DDslot)

            Names.DT <- DatadtToDT(DDlocal)[Variable == ExtractNames(VarNames)]
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            if(dim(Names.DT)[1] == 0) {

                out <- data.table(IDDD = character(0))
                output[[DDslot]] <- out

            } else {

                setnames(Names.DT, 'Variable', 'IDDD')
                Names.DT[, Sort := NULL]
                Names.DT[, Class := NULL]
                Names.DT[, Length := NULL]
                Names.DT[, ValueRegExp := NULL]

                ParsedNames <- strsplit(VarNames, '_')[[1]]
<<<<<<< HEAD
                IDQual <- DDlocal[Sort == 'IDQual'][['Variable']]
=======
                IDQual <- DatadtToDT(DDlocal)[Sort == 'IDQual'][['Variable']]
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
                IDQualCounter <- 0

                ColNames <- setdiff(names(Names.DT), 'IDDD')
                if (length(ColNames) > 0){
                    for (i in seq(along = ColNames)){

                        if (Names.DT[[paste0('Qual', i)]] %in% IDQual) {

                            Names.DT[, (paste0('Qual', i)) := NULL]
                            IDQualCounter <- IDQualCounter + 1
                            next
                        }

                        auxName <- Names.DT[[paste0('Qual', i)]]
                        if (auxName == '') {

                            Names.DT[, (paste0('Qual', i)) := NULL]
                            next
                        }
                        setnames(Names.DT, paste0('Qual', i), auxName)
                        Names.DT[, (auxName) := ParsedNames[i - IDQualCounter + 1]]
                    }
                }


                # Eliminamos columnas vacías
                for (col in names(Names.DT)){

                  Names.DT[is.na(get(col)), (col) := '']

                }

                output[[DDslot]] <- Names.DT

            }
        }
        outputGlobal <- Reduce(function(x, y){
                                merge(x, y, all = TRUE,
                                      by = intersect(names(x), names(y)))},
                                output)
        return(outputGlobal)

    } else { # Ahora para varias variables de entrada

        out.list <- lapply(as.list(VarNames), VarNamesToDT, DD = DD)

        out <- Reduce(function(x, y){
                            merge(x, y, all = TRUE, by = intersect(names(x), names(y)))},
                      out.list,
                      out.list[[1L]])

        # Pasamos NA a '' y eliminamos columnas vacías
        Cols <- sort(names(out))
        for (col in Cols){

            out[, (col) := ifelse(is.na(get(col)), '', get(col))]
            if (all(out[[col]] == '')) out[, (col) := NULL]

        }
        return(out)
    }
}