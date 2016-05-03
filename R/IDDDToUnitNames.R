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
#'                          .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                             NonIDQual = c(rep('',5)),
#'                                             IDDD = c('', 'Name', 'Surname', 'PostalAddr',
#'                                                  'PhoneNo'),
#'                                             NumIdEst = c('', rep('.', 4)),
#'                                             Unit1 = c('numidest', 'nombre', 'apellidos', 
#'                                                'direccion', 'telefono'))),
#'                 MicroData = new(Class = 'VNCdt',
#'                                 .Data =data.table(
#'                                        IDQual = c('NumIdEst', rep('', 4)),
#'                                        NonIDQual = c('', 'IsNatMarket', 
#'                                                      'IsEuroMarket', 
#'                                                      'IsRWMarket',
#'                                                      ''),
#'                                        IDDD = c(rep('', 4), 'NewOrders'),
#'                                        NumIdEst = c(rep('', 4), '.'),
#'                                        IsNatMarket = c(rep('', 4), '0'),
#'                                        IsEuroMarket = c(rep('', 4), '0'),
#'                                        IsRWMarket = c(rep('', 4), '1'),
#'                                        Unit1 = c('numidest', rep('', 3), 'cp09'))),
#'                 ParaData = new(Class = 'VNCdt',
#'                                 .Data = data.table(
#'                                        IDQual = c('NumIdEst', rep('', 2)),
#'                                        NonIDQual = c('', 'Action', ''),
#'                                        IDDD = c(rep('', 2), 'Date'),
#'                                        NumIdEst = c(rep('', 2), '.'),
#'                                        Action = c(rep('', 2), 'Imputation'),
#'                                        Unit1 = c('numidest', '', 'FechaImput'))),
#'                 Aggregates = new(Class = 'VNCdt',
#'                                  .Data = data.table(
#'                                         IDQual = c('Province', 'NACE09', '', ''),
#'                                         NonIDQual = c(rep('', 2), 'IsNatMarket', ''),
#'                                         IDDD = c('', '', '', 'Turnover'),
#'                                         Province = c('', '', '', '.'),
#'                                         NACE09 = c('', '', '', '.'),
#'                                         IsNatMarket = c('', '', '', '1'),
#'                                         Unit1 = c('provincia', 'actividad', '', 'cn01'))))
#'
#' VNC <- new(Class = 'VarNameCorresp', .Data = VarList)
#' 
#' ### We build the specification data.tables
#' IDdt <- new(Class = "DDdt", 
#'             .Data = data.table(
#'                      Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                      Sort = c('IDQual', rep('IDDD', 4)),
#'                      Class = rep('character', 5),
#'                      Qual1 = c('', rep('NumIdEst', 4)),
#'                      ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')))
#' Microdt <- new(Class = "DDdt", 
#'             .Data = data.table(
#'                      Variable = c('NumIdEst', 'IsNatMarket', 'IsEuroMarket', 
#'                       'IsRWMarket', 'NewOrders'),
#'                      Sort = c('IDQual', rep('NonIDQual', 3), 'IDDD'),
#'                      Class = c(rep('character', 4), 'numeric'),
#'                      Qual1 = c(rep('', 4), 'NumIdEst'),
#'                      ValueRegExp = c('[0-9]{9}PP', rep('(0|1| )', 3), '([0-9]{1, 10}| )')
#' ))
#' Paradt <- new(Class = "DDdt",
#'             .Data = data.table(
#'                      Variable = c('NumIdEst', 'Action', 'Date'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = rep('character', 3),
#'                      Qual1 = c(rep('', 2), 'NumIdEst'),
#'                      Qual2 = c(rep('', 2), 'Action'),
#'                      ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation', 
#'                      '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )')
#' ))
#' Aggdt <- new(Class = "DDdt", 
#'             .Data = data.table(
#'                      Variable = c('Province', 'NACE09', 'Turnover'),
#'                      Sort = c(rep('IDQual', 2), 'IDDD'),
#'                      Class = c(rep('character', 2), 'numeric'),
#'                      Qual1 = c(rep('', 2), 'Province'),
#'                      Qual2 = c(rep('', 2), 'NACE09'),
#'                      ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )')
#' ))
#' 
#' DD <- new(Class = 'DD', 
#'           VarNameCorresp = VNC, 
#'           ID = IDdt, 
#'           MicroData = Microdt, 
#'           ParaData = Paradt,
#'           Aggregates = Aggdt)
#' 
#' 
#' IDDDToUnitNames(VNC, IDDDNames = 'NewOrders_0_0_1')
#' 
#' IDDDToUnitNames(DD, IDDDNames = 'NewOrders_0_0_1')
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
    function(object, IDDDNames, Unit){
        
        if (missing(IDDDNames)) IDDDNames <- NULL
        if (missing(Unit)) Unit <- 'Unit1'
        
        XLS <- slot(object, '.Data')
        names(XLS) <- names(object)
        setDT(XLS)
        
        ColsUnit <- names(XLS)[grep('Unit', names(XLS))]
        ColsNotUnit <- setdiff(names(XLS), c(ColsUnit, getIDQual(object)))
        XLS[, IDDDName := '']
        
        for (col in ColsNotUnit) {
            
            XLS[, IDDDName := paste(IDDDName, get(col), sep = '_')]
            XLS[, IDDDName := gsub('^_+', '', IDDDName)]
            XLS[, IDDDName := gsub('_+$', '', IDDDName)]
            
        }
        
        if (is.null(IDDDNames)) {output <- XLS[which(XLS[[Unit]] != ""), 
                                               c('IDDDName', Unit), 
                                               with = F]
        
        } else {output <- XLS[IDDDName %in% IDDDNames,c('IDDDName', Unit), with = F]
        
        }
        
        
        return(output)
        
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
        
        output <- rbindlist(output)
        output <- output[!duplicated(output)] 
        
        return(output)
        
    }
    
)

#' @rdname IDDDToUnitNames
#'
#' @include DD-class.R 
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
#' @include StQ-class.R 
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


