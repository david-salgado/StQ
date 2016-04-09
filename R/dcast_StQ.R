#' @title Convert an \linkS4class{StQ} object into a dcasted data.table
#'
#' @description \code{dcast_StQ} returns a \linkS4class{data.table} in dcasted
#' form (observations by row and variables by columns) with data from the input
#' \linkS4class{StQ} object.
#'
#' This methods converts the slot \code{Data} from the input \code{StQ} object
#'  with key-value pair structure into a \linkS4class{data.table} with
#'  statistical units by row and variables specified in the input parameter
#'  \code{VarNames} by columns.
#'
#' To distinguish between variables and qualifiers this function makes use of
#' the slot \code{DD} of input \linkS4class{StQ} variable.
#'
#' This method is indeed a wrapper for the function
#' \code{\link[data.table]{dcast.data.table}} of the package
#' \linkS4class{data.table}, adapted to the structure of objecto
#' \linkS4class{StQ}.
#'
#' @param object Objecto of class \linkS4class{StQ} whose slot \code{Data} will
#' be converted.
#'
#' @param VarNames Character vector with names of the output variables.
#'
#' @param DDslot Character vector of length 1 with the name of DD slot used
#' to make the transformation of the input object. Its default value is
#' \code{MicroData}.
#'
#' @return \linkS4class{data.table} with data from slot \code{Data} of the input
#' \linkS4class{StQ} object with statistical units by rows and variables by
#'  columns. Only variables in \code{VarNames} will be output. If no variable
#'  name is specified, all variables in the input object will be output.
#'
#' @examples
#' data(ExampleQ)
#' Mat <- dcast_StQ(ExampleQ, c('IASSCifraNeg', 'IASSEmpleo_0',
#'                  'IASSEmpleo_1_1', 'IASSEmpleo_1_2'))
#' str(Mat)
#'
#' @seealso \code{\link{melt_StQ}}, \code{\link[data.table]{dcast.data.table}},
#' \code{\link[data.table]{melt.data.table}}, \code{\link[reshape2]{melt}},
#' \code{\link[reshape2]{dcast}}
#'
#' @export
setGeneric("dcast_StQ",
           function(object,
                    VarNames,
                    DDslot = 'MicroData'){standardGeneric("dcast_StQ")})

#' @rdname dcast_StQ
#'
#' @importFrom formula.tools lhs.vars
#'
#' @import data.table
#'
#' @include StQ-class.R getDD.R getData.R getUnits.R plus.DD.R VarNamesToFormula.R
#'
#' @export
setMethod(
    f = "dcast_StQ",
    signature = c("StQ"),
    function(object, VarNames, DDslot = 'MicroData'){

        if (length(DDslot) > 1) stop('[StQ::dcast_StQ] DDslot must be a character vector of length 1.')
        if (!DDslot %in% slotNames(getDD(object))) stop('[StQ::dcast_StQ] DDslot is not a component of the slot DD of the input object.')

        nQual <- length(setdiff(names(slot(getDD(object), DDslot)),
                                c('Variable', 'Sort', 'Class')))

        if (nQual == 0) stop('[StQ::dcast_StQ] The slot DD has no qualifiers.')

        DD <- getDD(object)
        IDQual <- (slot(DD, DDslot))[Sort == 'IDQual', Variable]
        NonIDQual <- (slot(DD, DDslot))[Sort == 'NonIDQual', Variable]

        Quals <- c(IDQual, NonIDQual)
        Quals <- intersect(VarNames, Quals)
        if (length(Quals) > 0){
            stop(paste0('[StQ::dcast_StQ]Variable ', Quals, ' is a qualifier in slot ', DDslot, ' of the slot DD of the input object. Please, remove it.'))
        }

        if (missing(VarNames)) {

            AllVar <- TRUE
            IDDDVarNames <- unique(getData(object)[['IDDD']])

        } else {

            AllVar <- FALSE
            IDDDVarNames <- VarNames
        }

        # Creamos una data.table auxDD con la fórmula asociada a cada variable según el slot DD
        auxDD <- VarNamesToFormula(IDDDVarNames, DD)


        # Se asocia a cada fórmula su correspondiente data.table dcasted
        auxData <- split(auxDD[['Variable']], auxDD[['Form']])

        dcastData <- lapply(as.list(names(auxData)), function(Form){

            if (AllVar) {

                aux <- getData(object)[IDDD %in% auxData[[Form]]]

            } else {

                aux <- getData(object, VarNames)[IDDD %in% auxData[[Form]]]

            }

            if (dim(aux)[[1]] == 0) return(NULL)
            setkeyv(aux, setdiff(names(aux), 'Value'))
            Dup <- aux[duplicated(aux)]
            if (dim(Dup)[[1]] > 0) {
              warning(paste0('[StQ::dcast_StQ] There exist duplicated rows in the component ',
                             Form,
                             '.\n The table will be reformatted with the default agg.fun function (length).\n'))
             }

            out <- data.table::dcast.data.table(data = aux,
                                                formula = as.formula(Form),
                                                drop = TRUE,
                                                value.var = 'Value')
            outNames <- sort(names(out))
            for (col in outNames){
              if (all(is.na(out[[col]]))) out[, col := NULL, with = F]
            }
            outNames <- names(out)
            outNewNames <- ifelse(substr(outNames,
                                         nchar(outNames),
                                         nchar(outNames)) == '_',
                                  substr(outNames,
                                         1,
                                         nchar(outNames) - 1),
                                  outNames)
            setnames(out, outNewNames)
            return(out)
        })
        names(dcastData) <- names(auxData)

        # Eliminamos componentes NULL de la lista de data.tables
        for (i in names(dcastData)){
          if (is.null(dcastData[[i]])) dcastData[[i]] <- NULL
          next
        }

#        IDQual <- unique(unlist(lapply(as.list(names(dcastData)), function(x){formula.tools::lhs.vars(as.formula(x))})))
        output <- Reduce(function(x, y){merge(x, y,
                                              by = intersect(intersect(IDQual,
                                                                       names(x)),
                                                             names(y)))},
                         dcastData)

        # Asignamos los tipos a cada variable

        DD <- slot(getDD(object), DDslot)
        outCols <- names(output)
        for (col in outCols){

            colClass <- copy(DD)[Variable == ExtractNames(col)][['Class']]
            output[, col := as(get(col), colClass), with = F]

        }
        return(output)
    }
)
