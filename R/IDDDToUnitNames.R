#' @title Convert IDDD variable names into their corresponding unit\emph{j} names
#'
#' @description \code{IDDDToUnitNames} returns a data table with the unit
#' variable name for each IDDD variable name.
#'
#' @param object Object with the IDDD variable identifiers.
#' 
#' @param IDDDNames character vector with the IDDD variables.
#' 
#' @param Unit character vector with the unit names.
#'
#' @return Data table with the IDDD variable names and their corresponding 
#' Unit\emph{j} names.
#'
#' @examples
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = new(Class = 'VNCdt',
#'                 data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                            NonIDQual = c('','','','',''),
#'                            IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                            NumIdEst = c('', rep('.', 4)),
#'                            UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'))
#'  ),
#' MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                            NonIDQual = c('', 'Market', ''),
#'                                            IDDD = c(rep('', 2), 'NewOrders'),
#'                                            NumIdEst = c(rep('', 2), '.'),
#'                                            Market = c(rep('', 2), '1.'),
#'                                            UnitName = c('numidest', '', 'cp09'))),
#' ParaData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                            NonIDQual = c('', 'Action', ''),
#'                                            IDDD = c(rep('', 2), 'Date'),
#'                                            NumIdEst = c(rep('', 2), '.'),
#'                                            Action = c(rep('', 2), 'Imputation'),
#'                                            UnitName = c('numidest', '', 'FechaImput'))),
#' AggWeights = new(Class = 'VNCdt', data.table(IDQual = c('CCAA', 'NACE09', ''),
#'                                            NonIDQual = rep('', 3),
#'                                            IDDD = c('', '', 'Ponderacion'),
#'                                            CCAA = c('', '', '.'),
#'                                            NACE09 = c('', '', '.'),
#'                                            UnitName = c('Provincia', '', ''))))
#' 
#' VNC <- new(Class = 'VarNameCorresp', VarList)
#' 
#' ### We build the specification data.tables
#' IDdt <- new( Class='DDdt',data.table(
#'     Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'     Sort = c('IDQual', rep('IDDD', 4)),
#'     Class = rep('character', 5),
#'     Length = c('11', '15', '15', '20','9'),
#'     Qual1 = c('', rep('NumIdEst', 4)),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')))
#' Microdt <- new( Class='DDdt',data.table(
#'     Variable = c('NumIdEst', 'Market', 'NewOrders'),
#'     Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'     Class = c(rep('character', 2), 'numeric'),
#'     Length = c('11', '2', '7'),
#'     Qual1 = c(rep('', 2), 'NumIdEst'),
#'     ValueRegExp = c('[0-9]{9}PP', '.+', '([0-9]{1, 10}| )')))
#' Paradt <-new( Class='DDdt', data.table(
#'     Variable = c('NumIdEst', 'Action', 'Date'),
#'     Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'     Class = rep('character', 3),
#'     Length = c('11', '10', '10'),
#'     Qual1 = c(rep('', 2), 'NumIdEst'),
#'     Qual2 = c(rep('', 2), 'Action'),
#'     ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation', 
#'                     '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )')))
#' Aggdt <- new(Class='DDdt',
#'              data.table(Variable = c('CCAA', 'NACE09', 'Ponderacion'),
#'                         Sort = c(rep('IDQual', 2), 'IDDD'),
#'                         Class = c(rep('character', 2), 'numeric'),
#'                         Length = c('2', '4', '7'),
#'                         Qual1 = c(rep('', 2), 'CCAA'),
#'                         Qual2 = c(rep('', 2), 'NACE09'),
#'                         ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', 
#'                                         '([0-9]{1, 15}| )')))
#' 
#' DD <- new(Class = 'DD', 
#'           VarNameCorresp = VNC, 
#'           ID = IDdt, 
#'           MicroData = Microdt, 
#'           ParaData = Paradt,
#'           Aggregates = Aggdt)
#' 
#' 
#' IDDDToUnitNames(VNC, IDDDNames = 'NewOrders_1.')
#' 
#' IDDDToUnitNames(DD, IDDDNames = 'Date_Imputation')
#'
#'
#' @export
setGeneric("IDDDToUnitNames", function(object, IDDDNames, Unit){standardGeneric("IDDDToUnitNames")})

#' @rdname IDDDToUnitNames
#'
#' @include VNCdt-class.R getIDQual.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "IDDDToUnitNames",
    signature = c("VNCdt"),
    function(object, IDDDNames){
        
        if (missing(IDDDNames)) IDDDNames <- NULL
        
        XLS <- slot(object, '.Data')
        names(XLS) <- names(object)
        setDT(XLS)

        ColsNotUnit <- setdiff(names(XLS), c('UnitName', getIDQual(object)))
        XLS[, IDDDName := '']
        
        for (col in ColsNotUnit) {
            
            XLS[, IDDDName := paste(IDDDName, get(col), sep = '_')]
            XLS[, IDDDName := gsub('^_+', '', IDDDName)]
            XLS[, IDDDName := gsub('_+$', '', IDDDName)]
            
        }

        if (is.null(IDDDNames)) {output <- XLS[which(XLS[['UnitName']] != ""), 
                                               c('IDDDName', 'UnitName'), 
                                               with = F]
        
        } else {
            
            output <- XLS[IDDDName %in% IDDDNames, c('IDDDName', 'UnitName'), with = F]
        
        }

        order <- match(output[[1]], IDDDNames)
        output[, order := order]
        setorder(output, order)
        output[, order := NULL]
        
        return(output[])
        
    }
    
)

#' @rdname IDDDToUnitNames
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "IDDDToUnitNames",
    signature = c("VarNameCorresp"),
    function(object, IDDDNames, Unit){
        
        VNCdtNames <- names(object)
        
        output <- list()
        for (Name in VNCdtNames) {
            
            out <- IDDDToUnitNames(object[[Name]], IDDDNames, Unit)
            
            if (dim(out)[1] > 0) {
                
                output[[Name]] <- out
                
            }else {
                
                next
            }
        }
        
        output <- rbindlist(output, fill = TRUE)
        output <- output[!duplicated(output)] 
        order <- match(output[[1]], IDDDNames)
        output[, order := order]
        setorder(output, order)
        output[, order := NULL]

        return(output)
        
    }
    
)

#' @rdname IDDDToUnitNames
#'
#' @include DD-class.R VarNameCorresp-class.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "IDDDToUnitNames",
    signature = c("DD"),
    function(object, IDDDNames, Unit){
        
        
        VNC <- slot(object, 'VarNameCorresp')
        
        output <- IDDDToUnitNames(VNC, IDDDNames, Unit)
        
        return(output)
        
    }
)

#' @rdname IDDDToUnitNames
#'
#' @include StQ-class.R DD-class.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "IDDDToUnitNames",
    signature = c("StQ"),
    function(object, IDDDNames, Unit){
        
        
        VNC <- slot(object@DD, 'VarNameCorresp')
        
        output <- IDDDToUnitNames(VNC, IDDDNames, Unit)
        
        return(output)
        
    }
)


