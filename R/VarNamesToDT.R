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
#' @include ExtractNames.R getVariables.R
#'
#' @import data.table
#' 
#' 
#' @export
VarNamesToDT <- function(VarNames, DD){
    
    IDDD <- NonIDQual <- Variable <- Sort <- Class <- Length <- ValueRegExp <- NULL
    
    NotPresentVar  <- setdiff(ExtractNames(VarNames), getVariables(DD))
    if (length(NotPresentVar) > 0) stop(paste0('[StQ::VarNamesToDD] The following variables are not contained in the DD slot: ', NotPresentVar, '.\n'))
    DotQual <- getDotQual(DD)
    DoubleDotQual <- getDoubleDotQual(DD)
    IDQual <- getIDQual(DD)
    completeVNC <- rbindlist(getVNC(DD), fill = TRUE)

    # Para una sola variable
    if (is.character(VarNames) & length(VarNames) == 1){
        
        tempVNC <- completeVNC[IDDD == ExtractNames(VarNames) | IDQual == ExtractNames(VarNames) | NonIDQual == ExtractNames(VarNames)]
        tempVNC <- tempVNC[!duplicated(tempVNC, by = c('UnitName'))]
        tempVNC <- tempVNC[, which(sapply(tempVNC, function(x)!all(is.na(x)))), with = FALSE]
        
        if (!all(tempVNC[['IDQual']] == '')) IDvar <- 'IDQual'
        if (!all(tempVNC[['NonIDQual']] == '')) IDvar <- 'NonIDQual'
        if (!all(tempVNC[['IDDD']] == '')) IDvar <- 'IDDD'
        
        DDSlotNames <- setdiff(names(DD), 'VNC')

        output <- list()

        for (DDslot in DDSlotNames){

            DDlocal <- DD[[DDslot]]

            Names.DT <- DDlocal[Variable == ExtractNames(VarNames)]

            if(dim(Names.DT)[1] == 0) {

                out <- data.table(IDDD = character(0))
                setnames(out, 'IDDD', IDvar)
                output[[DDslot]] <- out

            } else {

                setnames(Names.DT, 'Variable', IDvar)
                Names.DT[, Sort := NULL]
                Names.DT[, Class := NULL]
                Names.DT[, Length := NULL]
                Names.DT[, ValueRegExp := NULL]
                Qual_w_Values <- unlist(Names.DT)
                Qual_w_Values <- Qual_w_Values[which(!Qual_w_Values %in% c(ExtractNames(VarNames), DotQual))]
                Qual_w_Values <- Qual_w_Values[Qual_w_Values != '']

                localVNC <- copy(tempVNC)[, c(IDvar, Qual_w_Values), with = FALSE]

                ParsedNames <- stringr::str_split(VarNames, '_')[[1]]
                ParsedNames <- ParsedNames[-1]
                
                
                for (index_col in seq_along(Qual_w_Values)){
                    
                    
                    col <- Qual_w_Values[index_col]

                    if ('..' %in% localVNC[[col]]) {


                        localVNC[, (col) := paste0(ParsedNames[index_col:(length(ParsedNames))], collapse = '_')]
                        next
                    }
                    
                    localVNC <- localVNC[get(col) == ParsedNames[index_col]]
                    
                }
                
                # localVNC <- localVNC[, which(sapply(localVNC, function(x)all(x != ''))), with = FALSE]
                # localVNC <- localVNC[, which(sapply(localVNC, function(x)!all(is.na(x)))), with = FALSE]
         
                output[[DDslot]] <- localVNC

                # IDQualCounter <- 0
                # 
                # ColNames <- setdiff(names(Names.DT), 'IDDD')
                # 
                # if (length(ColNames) > 0){
                #     for (i in seq(along = ColNames)){
                # 
                #         if (Names.DT[[paste0('Qual', i)]] %in% DotQual) {
                # 
                #             Names.DT[, (paste0('Qual', i)) := NULL]
                #             IDQualCounter <- IDQualCounter + 1
                #             next
                #         }
                # 
                #         auxName <- Names.DT[[paste0('Qual', i)]]
                #         if (auxName == '') {
                # 
                #             Names.DT[, (paste0('Qual', i)) := NULL]
                #             next
                #         }
                #         setnames(Names.DT, paste0('Qual', i), auxName)
                #         Names.DT[, (auxName) := ParsedNames[i - IDQualCounter + 1]]
                # }
# 
# 
#                     # Eliminamos columnas vacías
#                     for (col in names(Names.DT)){
# 
#                         Names.DT[is.na(get(col)), (col) := '']
# 
#                     }
# 
#                     output[[DDslot]] <- Names.DT
#                 }


            }
        }

        outputGlobal <- rbindlist(output, fill = TRUE)
        outputGlobal <- outputGlobal[!duplicated(outputGlobal, by = names(outputGlobal))]
        return(outputGlobal[])

    } else { # Ahora para varias variables de entrada

        out.list <- lapply(as.list(VarNames), VarNamesToDT, DD = DD)

        out <- rbindlist(out.list, fill = TRUE)

        # Pasamos NA a '' y eliminamos columnas vacías
        Cols <- sort(names(out))
        for (col in Cols){

            out[, (col) := ifelse(is.na(get(col)), '', get(col))]
            # if (all(out[[col]] == '')) out[, (col) := NULL]

        }
        return(out[])
    }
}
