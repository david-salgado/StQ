#' @title Convert unit\emph{j} names into their corresponding IDDD variable names.  
#'
#' @description \code{UnitToIDDDNames} returns a data table with the IDDD variable name
#' corresponding to the Unit\emph{j} variable name specified .
#'
#' @param object Object with the IDDD variable identifiers.
#' 
#' @param Unit character vector with the Unit name ('Unit1', 'Unit2', ...).
#' 
#' @param UnitNames character vector with the name of variable corresponding to the specified Unit.
#'
#' @return Data table with all the corresponding IDDD variable names. For objects the classes
#' \linkS4class{DD} and \linkS4class{StQ} it returns the IDDD the slot VarNameCorresp of the
#' corresponding DD object.
#'
#' @examples
#' # An example for VNCdt:
#' library(data.table)
#' VNCdt <- new(Class = 'VNCdt',
#'                 .Data =data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                   NonIDQual = c('', 'Market', '', ''),
#'                                   IDDD = c(rep('', 2), 'NewOrders'),
#'                                   NumIdEst = c(rep('', 2), '.'),
#'                                   Market = c(rep('', 2), '1.'),
#'                                   Unit1 = c('numidest', '', 'cp09')))
#'  
#'  UnitToIDDDNames(VNCdt, Unit = 'Unit1', UnitNames = c('cp09'))
#'
#' # An example for VNC and DD objects:
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = new(Class = 'VNCdt',
#'                          .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                             NonIDQual = c(rep('',5)),
#'                                             IDDD = c('', 'Name', 'Surname', 'PostalAddr',
#'                                                      'PhoneNo'),
#'                                             NumIdEst = c('', rep('.', 4)),
#'                                             Unit1 = c('numidest', 'nombre', 'apellidos', 
#'                                                       'direccion', 'telefono')     
#'                                 )),
#'                 MicroData = new(Class = 'VNCdt',
#'                                 .Data = data.table(
#'                                         IDQual = c('NumIdEst', rep('', 2)),
#'                                         NonIDQual = c('', 'Market', ''),
#'                                         IDDD = c(rep('', 2), 'NewOrders'),
#'                                         NumIdEst = c(rep('', 2), '.'),
#'                                         Market = c(rep('', 2), '2.'),
#'                                         Unit1 = c('numidest', '', 'cp09'))),
#'                 ParaData = new(Class = 'VNCdt',
#'                                 .Data = data.table(
#'                                         IDQual = c('NumIdEst', rep('', 2)),
#'                                         NonIDQual = c('', 'Action', ''),
#'                                         IDDD = c(rep('', 2), 'Date'),
#'                                         NumIdEst = c(rep('', 2), '.'),
#'                                         Action = c(rep('', 2), 'Imputation'),
#'                                         Unit1 = c('numidest', '', 'FechaImput'))),
#'                 Aggregates = new(Class = 'VNCdt',
#'                                  .Data = data.table(
#'                                          IDQual = c('Province', 'NACE09', '', ''),
#'                                          NonIDQual = c(rep('', 2), 'Market', ''),
#'                                          IDDD = c('', '', '', 'Turnover'),
#'                                          Province = c('', '', '', '.'),
#'                                          NACE09 = c('', '', '', '.'),
#'                                          Market = c('', '', '', '3.'),
#'                                          Unit1 = c('provincia', 'actividad', '', 'cn01'))))
#'
#' VNC <- new(Class = 'VarNameCorresp', .Data = VarList)
#' 
#' ### We build the specification data.tables
#' IDdt <- new(Class = "DDdt", 
#'             .Data = data.table(
#'                      Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                      Sort = c('IDQual', rep('IDDD', 4)),
#'                      Class = rep('character', 5),
#'                      Length = c('11', '25', '25', '50', '9'),
#'                      Qual1 = c('', rep('NumIdEst', 4)),
#'                      ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')))
#' Microdt <- new(Class = "DDdt", 
#'             .Data = data.table(
#'                      Variable = c('NumIdEst', 'Market', 'NewOrders'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = c(rep('character', 2), 'numeric'),
#'                      Length = c('11', '2', '7'),
#'                      Qual1 = c(rep('', 2), 'NumIdEst'),
#'                      Qual2 = c(rep('', 4), 'Market'),
#'                      ValueRegExp = c('[0-9]{9}PP', '(0|1| )', '([0-9]{1, 10}| )')))
#' Paradt <- new(Class = "DDdt",
#'             .Data = data.table(
#'                      Variable = c('NumIdEst', 'Action', 'Date'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = rep('character', 3),
#'                      Length = c('11', '4', '10'),
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
#'                      Length = c('25', '4', '12'),
#'                      Qual1 = c(rep('', 2), 'Province'),
#'                      Qual2 = c(rep('', 2), 'NACE09'),
#'                      ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )')))
#' 
#' DD <- new(Class = 'DD', 
#'           VarNameCorresp = VNC, 
#'           ID = IDdt, 
#'           MicroData = Microdt, 
#'           ParaData = Paradt,
#'           Aggregates = Aggdt)
#' 
#' 
#' UnitToIDDDNames(VNC, Unit = 'Unit1', UnitNames = 'cp09')
#' 
#' UnitToIDDDNames(DD, Unit = 'Unit1', UnitNames = 'cp09')
#'
#'
#' @export
setGeneric("UnitToIDDDNames", function(object, Unit, UnitNames){standardGeneric("UnitToIDDDNames")})

#' @rdname UnitToIDDDNames
#'
#' @include VNCdt-class.R getIDQual.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "UnitToIDDDNames",
    signature = c("VNCdt"),
    function(object, Unit, UnitNames){
        
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
        
        output <- XLS[which(XLS[[Unit]] == UnitNames), c(Unit,'IDDDName'), with = F]
        
        
        return(output)
        
    }
    
)

#' @rdname UnitToIDDDNames
#'
#' @include VarNameCorresp-class.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "UnitToIDDDNames",
    signature = c("VarNameCorresp"),
    function(object, Unit, UnitNames){
        
        VNCdtNames <- names(object)
        
        output <- list()
        for (Name in VNCdtNames) {
            
            out <- UnitToIDDDNames(object[[Name]], Unit, UnitNames)
            
            if (length(out) > 0) {
                
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

#' @rdname UnitToIDDDNames
#'
#' @include DD-class.R VarNamesToDD.R getVNC.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "UnitToIDDDNames",
    signature = c("DD"),
    function(object, Unit, UnitNames){
        
        
        VNC <- getVNC(object)
        
        output <- UnitToIDDDNames(VNC, Unit, UnitNames)

        #aux <- VarNamesToDD(output[['IDDDName']], DD)
        #output <- cbind(output, aux)
        
        return(output)
        
    }
)

#' @rdname UnitToIDDDNames
#'
#' @include StQ-class.R 
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "UnitToIDDDNames",
    signature = c("StQ"),
    function(object, Unit, UnitNames){
        
        
        DD <- object@DD
        
        output <- UnitToIDDDNames(DD, Unit, UnitNames)
        
        return(output)
        
    }
)
