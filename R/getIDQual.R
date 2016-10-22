#' @title Return IDDD identifiers from an object
#'
#' @description \code{getIDQual} returns a character vector with all unit qualifier
#' names (IDQual) included in the input object.
#'
#' @param object Object with the IDQual unit qualifier.
#'
#' @param Namesdt Character vector with the components or slots from which IDQuals
#' are requiered.
#'
#' @return Character vector with all the unit qualifier names.
#'
#' @examples
#' library(data.table)
#' VarList <- list(ID = new(Class = 'VNCdt',
#'                 data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                            NonIDQual = c('','','','',''),
#'                            IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                            NumIdEst = c('', rep('.', 4)),
#'                            UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                            InFiles = rep('FI', 5))),
#' MicroData =new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                            NonIDQual = c('', 'Market', ''),
#'                                            IDDD = c(rep('', 2), 'NewOrders'),
#'                                            NumIdEst = c(rep('', 2), '.'),
#'                                            Market = c(rep('', 2), '1.'),
#'                                            UnitName = c('numidest', '', 'cp09'),
#'                                            InFiles = rep('FF, FD, FG', 3))),
#' ParaData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                            NonIDQual = c('', 'Action', ''),
#'                                            IDDD = c(rep('', 2), 'Date'),
#'                                            NumIdEst = c(rep('', 2), '.'),
#'                                            Action = c(rep('', 2), 'Imputation'),
#'                                            UnitName = c('numidest', '', 'FechaImput'),
#'                                            InFiles = rep('FP', 3))),
#' AggWeights = new(Class = 'VNCdt', data.table(IDQual = c('CCAA', 'NACE09', ''),
#'                                            NonIDQual = rep('', 3),
#'                                            IDDD = c('', '', 'Ponderacion'),
#'                                            CCAA = c('', '', '.'),
#'                                            NACE09 = c('', '', '.'),
#'                                            UnitName = c('Provincia', '', ''),
#'                                            InFiles = rep('FA', 3))))
#' VNC <- new(Class = 'VarNameCorresp', .Data = VarList)
#' getIDQual(VNC)
#'
#'
#' @export
setGeneric("getIDQual", function(object, Namesdt){standardGeneric("getIDQual")})

#' @rdname getIDQual
#'
#' @include VNCdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("VNCdt"),
    function(object){

        output <- unique(object[['IDQual']])
        output <- output[output != '']
        return(output)

    }
)
#' @rdname getIDQual
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("VarNameCorresp"),
    function(object, Namesdt){

        if (missing(Namesdt)) Namesdt <- names(object)

        aux <- object[Namesdt]

        IDQual.list <- lapply(aux, function(x) {
            IDQual <- getIDQual(x)
            return(IDQual)
        }
        )

        output <- unique(Reduce(c, IDQual.list, init = IDQual.list[[1]]))
        return(output)

    }
)

#' @rdname getIDQual
#'
#' @include DDdt-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("DDdt"),
    function(object){

        output <- unique(object[Sort == 'IDQual', Variable])
        output <- output[output != '']

        return(output)
    }
)

#' @rdname getIDQual
#'
#' @include DD-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("DD"),
    function(object, Namesdt){

        if (missing(Namesdt)) Namesdt <- slotNames(object)

        Namesdt <- setdiff(Namesdt, 'VarNameCorresp')
        output <- c()
        for (DDdt in Namesdt) {

            IDQual <- getIDQual(slot(object, DDdt))
            output <- c(output, IDQual)
        }

        output <- unique(output)
        return(output)
    }
)

#' @rdname getIDQual
#'
#' @include StQ-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "getIDQual",
    signature = c("StQ"),
    function(object, Namesdt){

        if (missing(Namesdt)) {output <- getIDQual(object@DD)
        } else{output <- getIDQual(object@DD, Namesdt)}
        
        output <- intersect(names(object@Data), output)
        return(output)
    }
)
