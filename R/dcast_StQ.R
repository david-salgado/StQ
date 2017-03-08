#' @title Convert an \linkS4class{StQ} object into a dcasted \linkS4class{data.table}
#'
#' @description \code{dcast_StQ} returns a \linkS4class{data.table} in dcasted form (observations by
#' row and variables by columns) with data from the input \linkS4class{StQ} object.
#'
#' This method converts the slot \code{Data} from the input \code{StQ} object into a
#' \linkS4class{data.table} with statistical units by row and variables specified in the input
#' parameter \code{VarNames} by columns.
#'
#' To distinguish between variables and qualifiers this function makes use of the slot \code{DD} of
#' input \linkS4class{StQ} variable.
#'
#' This method is indeed a wrapper for the function \code{\link[data.table]{dcast.data.table}} of
#' the package \linkS4class{data.table}, adapted to the structure of object \linkS4class{StQ}.
#'
#' @param object Object of class \linkS4class{StQ} whose slot \code{Data} will be converted.
#'
#' @param VarNames \code{Character} vector with names of the output variables.
#'
#' @return Returns a \linkS4class{data.table} with data from slot \code{Data} of the input
#' \linkS4class{StQ} object with statistical units by rows and variables by columns. Only variables
#' in \code{VarNames} will be output. If no variable name is specified, all variables in the input
#' object will be output.
#'
#' @examples
#' data(ExampleStQ)
#' Mat <- dcast_StQ(ExampleStQ, VarNames = 'Turnover')
#' Mat
#' str(Mat)
#'
#' dcast_StQ(ExampleStQ, VarNames = 'Employees')
#'
#' dcast_StQ(ExampleStQ[ID != ''])
#'
#' @seealso \code{\link{melt_StQ}}, \code{\link[data.table]{dcast.data.table}},
#' \code{\link[data.table]{melt.data.table}}, \code{\link[reshape2]{melt}},
#' \code{\link[reshape2]{dcast}}
#'
<<<<<<< HEAD
#' @include StQ.R DDslotWith.R getNonIDQual.R getDD.R getData.R getVNC.R getIDQual.R VarNamesToFormula.R sub.StQ.R getIDDD.R ExtractNames.R
#' 
=======
#' @export
setGeneric("dcast_StQ",
           function(object, VarNames = NULL){standardGeneric("dcast_StQ")})

#' @rdname dcast_StQ
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @importFrom formula.tools lhs.vars
#'
#' @importFrom stats as.formula
#'
<<<<<<< HEAD
#' @import data.table
#'
#' @export
setGeneric("dcast_StQ",
           function(object, VarNames = NULL){standardGeneric("dcast_StQ")})

#' @rdname dcast_StQ
=======
#' @import data.table methods
#'
#' @include StQ-class.R DDslotWith.R getNonIDQual.R getDD.R getData.R getVNC.R getIDQual.R getNonIDQual.R VarNamesToFormula.R subset.StQ.R getIDDD.R ExtractNames.R DatadtToDT.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'
#' @export
setMethod(
    f = "dcast_StQ",
    signature = c("StQ"),
    function(object, VarNames = NULL){

<<<<<<< HEAD
        DD <- getDD(object)
        VNC <- getVNC(DD)
        
        DDdt.list <- setdiff(names(DD), 'VNC')
        DDdt.list <- lapply(DDdt.list, function(Name){DD[[Name]]})
        DDdt <- rbindlist(DDdt.list, fill = TRUE)
=======
        VNC <- getVNC(object)
        DD <- getDD(object)

        DDdt.list <- setdiff(slotNames(DD), 'VarNameCorresp')
        DDdt.list <- lapply(DDdt.list, function(Name){slot(DD, Name)})
        DDdt <- Reduce('+', DDdt.list, init = DDdt.list[[1]])
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8

        for (VarName in VarNames){

            if (VarName != ExtractNames(VarName)) stop('[StQ::dcast_StQ] Only variable names without qualifiers are allowed in VarNames. If you are interested in a particular column, subset the output dcasted data.table.\n')
        }

<<<<<<< HEAD
        IDQual <- getIDQual(DD)
        NonIDQual <- getNonIDQual(DD)
=======
        IDQual <- getIDQual(DDdt)
        NonIDQual <- getNonIDQual(DDdt)
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
        Quals <- c(IDQual, NonIDQual)
        Quals <- intersect(VarNames, Quals)
        if (length(Quals) == 1){

            stop(paste0('[StQ::dcast_StQ] The input variable name ', Quals, ' is a qualifier in the input object. Please, remove it from the call.\n'))

        } else if (length(Quals) > 1){

            stop(paste0('[StQ::dcast_StQ] The input variable names ', Quals, ' are qualifiers in the input object. Please, remove them from the call.\n'))
        }

        if (is.null(VarNames)) {

            AllVar <- TRUE
            IDDDVarNames <- getIDDD(object)

        } else {

            AllVar <- FALSE
            IDDDVarNames <- VarNames
        }

        # Creamos una data.table auxDD con la fórmula asociada a cada variable según el slot DD
        auxDD <- VarNamesToFormula(IDDDVarNames, DD)

        # Se asocia a cada fórmula su correspondiente data.table dcasted
        auxData <- split(auxDD[['Variable']], auxDD[['Form']])

        dcastData <- lapply(names(auxData), function(Form){

            #Preparamos la data.table aux que vamos a reformatear con dcast.data.table
<<<<<<< HEAD
            aux <- getData(object)[IDDD %in% auxData[[Form]]]
=======
            aux <- DatadtToDT(getData(object))[IDDD %in% auxData[[Form]]]
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            if (dim(aux)[[1]] == 0) return(NULL)

            ColNames <- names(aux)
            setkeyv(aux, setdiff(ColNames, 'Value'))
            Dup <- aux[duplicated(aux, by = key(aux))]
            if (dim(Dup)[[1]] > 0) {

              warning(paste0('[StQ::dcast_StQ] There exist duplicated rows in the component ',
                             Form,
                             '.\n The table will be reformatted with the default agg.fun function (length).\n'))
            }
<<<<<<< HEAD

            FormVars <- all.vars(as.formula(Form))
            MissingQuals <- setdiff(FormVars, ColNames)
            if (length(MissingQuals) > 0) {

                aux[, (MissingQuals) := '']
            }

=======

            FormVars <- all.vars(as.formula(Form))
            MissingQuals <- setdiff(FormVars, ColNames)
            if (length(MissingQuals) > 0) {

                aux[, (MissingQuals) := '']
            }

>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            aux <- aux[, c(FormVars, 'Value'), with = F]
            out <- data.table::dcast.data.table(data = aux,
                                                formula = as.formula(Form),
                                                drop = TRUE,
                                                value.var = 'Value')

            outNames <- sort(names(out))
            for (col in outNames){
              if (all(is.na(out[[col]]))) out[, (col) := NULL]
              if (col == '.') out[, (col) := NULL]
            }
            return(out)
        })
        names(dcastData) <- names(auxData)

        # Eliminamos componentes NULL de la lista de data.tables transformadas
        for (i in names(dcastData)){
          if (is.null(dcastData[[i]])) dcastData[[i]] <- NULL
          next
        }

        # Combinamos las data.tables de la lista en una sola data.table
        output <- Reduce(
            function(x, y){

                CommonCols <- intersect(names(x), names(y))
                if (length(CommonCols) > 0) {

                    out <- merge(x, y, by = CommonCols, all = TRUE)

                } else {

                    out <- rbindlist(list(x, y), fill = TRUE)

                }
                ColNames <- names(out)
                for (col in ColNames){

                    out[is.na(get(col)), (col) := '']
                }
                return(out)
            },
            dcastData,
            init = dcastData[[1]])

        # Asignamos los tipos a cada variable sustituyendo blancos por NA
        outCols <- names(output)
        for (col in outCols){

<<<<<<< HEAD
            colClass <- unique(DDdt[Variable == ExtractNames(col)][['Class']])
=======
            colClass <- DatadtToDT(DDdt)[Variable == ExtractNames(col)][['Class']]
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            output[, (col) := as(get(col), colClass)]
            output[get(col) == '', (col) := NA]

        }
        return(output[])
    }
)
