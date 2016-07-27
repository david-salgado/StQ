#' @title Convert an \linkS4class{StQ} object into a dcasted data.table.
#'
#' @description \code{dcast_StQ} returns a \linkS4class{data.table} in dcasted form (observations by
#' row and variables by columns) with data from the input \linkS4class{StQ} object.
#'
#' This methods converts the slot \code{Data} from the input \code{StQ} object into a
#' \linkS4class{data.table} with statistical units by row and variables specified in the input
#' parameter \code{VarNames} by columns.
#'
#' To distinguish between variables and qualifiers this function makes use of the slot \code{DD} of
#' input \linkS4class{StQ} variable.
#'
#' This method is indeed a wrapper for the function
#' \code{\link[data.table]{dcast.data.table}} of the package
#' \linkS4class{data.table}, adapted to the structure of objecto
#' \linkS4class{StQ}.
#'
#' @param object Object of class \linkS4class{StQ} whose slot \code{Data} will be converted.
#'
#' @param VarNames Character vector with names of the output variables.
#'
#' @return \linkS4class{data.table} with data from slot \code{Data} of the input \linkS4class{StQ}
#' object with statistical units by rows and variables by columns. Only variables in \code{VarNames}
#' will be output. If no variable name is specified, all variables in the input object will be
#' output.
#'
#' @examples
#' data(ExampleStQ)
#' Mat <- dcast_StQ(ExampleStQ, VarNames = c('Turnover'))
#' str(Mat)
#'
#' @seealso \code{\link{melt_StQ}}, \code{\link[data.table]{dcast.data.table}},
#' \code{\link[data.table]{melt.data.table}}, \code{\link[reshape2]{melt}},
#' \code{\link[reshape2]{dcast}}
#'
#' @export
setGeneric("dcast_StQ",
           function(object,
                    VarNames = NULL){standardGeneric("dcast_StQ")})

#' @rdname dcast_StQ
#'
#' @importFrom formula.tools lhs.vars
#'
#' @importFrom stats as.formula
#'
#' @import data.table methods
#'
#' @include StQ-class.R DDslotWith.R getNonIDQual.R getDD.R getData.R getDD.R VarNamesToFormula.R
#'
#' @export
setMethod(
    f = "dcast_StQ",
    signature = c("StQ"),
    function(object, VarNames = NULL){

        VNC <- getVNC(object)
        DD <- getDD(object)
        
        DDdt.list <- setdiff(slotNames(DD), 'VarNameCorresp')
        DDdt.list <- lapply(DDdt.list, function(Name){slot(DD, Name)})
        DDdt <- Reduce('+', DDdt.list, init = DDdt.list[[1]])
        
        for (VarName in VarNames){
            
            Quals <- setdiff(names(DDdt), c('Variable', 'Sort', 'Class', 'Length', 'ValueRegExp'))
            
            NameQuals <- c()
            for (Qual in Quals){
                
                NameQuals <- c(NameQuals, DDdt[Variable == ExtractNames(VarName)][[Qual]])
            }
            
            nonIDQuals <- getNonIDQual(DDdt)
            
            
            if (!all(NameQuals %in% nonIDQuals) & VarName != ExtractNames(VarName)){
                
                stop('[StQ::dcast_StQ] Variable ', ExtractNames(VarName), ' has not any non-identity qualifiers, so VarName cannot be ', VarName, '.')
            }
        }
        
        IDQual <- DDdt[Sort == 'IDQual', Variable]
        NonIDQual <- DDdt[Sort == 'NonIDQual', Variable]

        Quals <- c(IDQual, NonIDQual)
        Quals <- intersect(VarNames, Quals)
        if (length(Quals) > 0){
            stop(paste0('[StQ::dcast_StQ]Variable ', Quals, ' is a qualifier in slot ', DDdt, ' of the slot DD of the input object. Please, remove it.'))
        }

        if (is.null(VarNames)) {

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
            
            
            aux <- getData(object)[IDDD %in% auxData[[Form]]]
            
            if (dim(aux)[[1]] == 0) return(NULL)
            
            ColNames <- names(aux)
            NotEmptyCols <- c()
            for (col in setdiff(ColNames, 'Value')){
                
                if (!all(is.na(aux[[col]]) | aux[[col]] == '')) NotEmptyCols <- c(NotEmptyCols, col)
            }
            aux <- aux[, unique(c(NotEmptyCols, 'Value')), with = F]
           
            setkeyv(aux, setdiff(names(aux), 'Value'))
            Dup <- aux[duplicated(aux)]
            if (dim(Dup)[[1]] > 0) {
              warning(paste0('[StQ::dcast_StQ] There exist duplicated rows in the component ',
                             Form,
                             '.\n The table will be reformatted with the default agg.fun function (length).\n'))
             }

            Form <-  unlist(strsplit(Form, ' + ', fixed = T, useBytes = T))
            cals <- intersect(names(aux), Form)
            
            
            Form <- Form[1]
            Form <- paste0(c(Form, cals), collapse = ' + ')
            out <- data.table::dcast.data.table(data = aux,
                                                formula = as.formula(Form),
                                                drop = TRUE,
                                                value.var = 'Value')
            outNames <- sort(names(out))
            for (col in outNames){
              if (all(is.na(out[[col]]))) out[, col := NULL, with = F]
            }
            return(out)
        })
        names(dcastData) <- names(auxData)

        # Eliminamos componentes NULL de la lista de data.tables
        for (i in names(dcastData)){
          if (is.null(dcastData[[i]])) dcastData[[i]] <- NULL
          next
        }

        output <- Reduce(
            function(x, y){ merge(x, y, by = intersect(names(x), names(y)))}, 
            dcastData, init = dcastData[[1]])
        
        # Asignamos los tipos a cada variable

        outCols <- names(output)
        for (col in outCols){
            
            colClass <- copy(DDdt)[Variable == ExtractNames(col)][['Class']]
            output[, col := as(get(col), colClass), with = F]

        }
        return(output)
    }
)
