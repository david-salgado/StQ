#' @title Convert unit\emph{j} names into their corresponding IDDD variable names.  
#'
#' @description \code{UnitToIDDDNames} returns a data table with the IDDD variable name
#' corresponding to the Unit\emph{j} variable name specified .
#'
#' @param UnitNames character vector with the name of variable corresponding to the specified Unit.
#'
#' @param Correspondence Object with the IDDD variable identifiers.
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
#'                                   NonIDQual = c('', 'Market', ''),
#'                                   IDDD = c(rep('', 2), 'NewOrders'),
#'                                   NumIdEst = c(rep('', 2), '.'),
#'                                   Market = c(rep('', 2), '1.'),
#'                                   UnitName = c('numidest', '', 'cp09')))
#'  
#'  UnitToIDDDNames(VNCdt, UnitNames = c('cp09'))
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
#'                                             UnitName = c('numidest', 'nombre', 'apellidos', 
#'                                                       'direccion', 'telefono')     
#'                                 )),
#'                 MicroData = new(Class = 'VNCdt',
#'                                 .Data = data.table(
#'                                         IDQual = c('NumIdEst', rep('', 2)),
#'                                         NonIDQual = c('', 'Market', ''),
#'                                         IDDD = c(rep('', 2), 'NewOrders'),
#'                                         NumIdEst = c(rep('', 2), '.'),
#'                                         Market = c(rep('', 2), '2.'),
#'                                         UnitName = c('numidest', '', 'cp09'))),
#'                 ParaData = new(Class = 'VNCdt',
#'                                 .Data = data.table(
#'                                         IDQual = c('NumIdEst', rep('', 2)),
#'                                         NonIDQual = c('', 'Action', ''),
#'                                         IDDD = c(rep('', 2), 'Date'),
#'                                         NumIdEst = c(rep('', 2), '.'),
#'                                         Action = c(rep('', 2), 'Imputation'),
#'                                         UnitName = c('numidest', '', 'FechaImput'))),
#'                 Aggregates = new(Class = 'VNCdt',
#'                                  .Data = data.table(
#'                                          IDQual = c('Province', 'NACE09', '', ''),
#'                                          NonIDQual = c(rep('', 2), 'Market', ''),
#'                                          IDDD = c('', '', '', 'Turnover'),
#'                                          Province = c('', '', '', '.'),
#'                                          NACE09 = c('', '', '', '.'),
#'                                          Market = c('', '', '', '3.'),
#'                                          UnitName = c('provincia', 'actividad', '', 'cn01'))))
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
#'                      Qual2 = c(rep('', 2), 'Market'),
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
#' UnitToIDDDNames(VNC, UnitNames = 'cp09')
#' 
#' UnitToIDDDNames(DD, UnitNames = 'cp09')
#'
#'
#' @export
setGeneric("UnitToIDDDNames", function(UnitNames, Correspondence){standardGeneric("UnitToIDDDNames")})

#' @rdname UnitToIDDDNames
#'
#' @include VNCdt-class.R getIDQual.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "UnitToIDDDNames",
    signature = c("character", "VNCdt"),
    function(UnitNames, Correspondence){
        
        XLS <- slot(Correspondence, '.Data')
        names(XLS) <- names(Correspondence)
        setDT(XLS)
        
        XLS[, IDDDName := IDDD]
        XLS.Quals <- XLS[IDDD == '']
        XLS.Quals[IDQual != '', IDDDName := IDQual]
        XLS.Quals[NonIDQual != '', IDDDName := NonIDQual]
        XLS.Quals <- XLS.Quals[, c('IDQual', 'NonIDQual', 'UnitName', 'IDDDName'), with = F]
        XLS <- XLS[IDDD != '']
        XLS.list <- split(XLS, XLS[['IDDD']])
        XLS.list <- lapply(XLS.list, function(xls){
            
            ColNames <- names(xls)
            NotEmptyCols <- c()
            for (col in ColNames){
                
                if (!all(is.na(xls[[col]]) | xls[[col]] == '')) NotEmptyCols <- c(NotEmptyCols, col)

            }
            xls <- xls[, NotEmptyCols, with = F]
            ColsNotUnit <- setdiff(names(xls), c('IDDD', 'UnitName', 'IDDDName'))
            for (col in ColsNotUnit) {
                
                if (all(xls[[col]] == '.') | all(is.na(xls[[col]]))) next
                xls[, IDDDName := paste(IDDDName, get(col), sep = '_')]
                
            }    
            return(xls)
        })

        
        output <- rbindlist(XLS.list, fill = TRUE)
        output <- rbindlist(list(output, XLS.Quals), fill = TRUE)
        
        output <- output[which(output[['UnitName']] %in% UnitNames), c('UnitName','IDDDName'), with = F]

        out <- output[['IDDDName']]
        names(out) <- output[['UnitName']]
        out <- out[UnitNames]
        return(out)
        
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
    signature = c("character", "VarNameCorresp"),
    function(UnitNames, Correspondence){
        
        VNCdtNames <- names(Correspondence)
        
        output <- list()
        for (Name in VNCdtNames) {
            
            localUnitNames <- intersect(UnitNames, Correspondence[[Name]][['UnitName']])
            out <- UnitToIDDDNames(localUnitNames, Correspondence[[Name]])
            
            if (length(out) > 0) {
                
                output[[Name]] <- out
                
            } else {
                
                next
            }
        }
        names(output) <- NULL
        output <- unlist(output)
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
    signature = c("character", "DD"),
    function(UnitNames, Correspondence){
        
        
        VNC <- getVNC(Correspondence)
        
        output <- UnitToIDDDNames(UnitNames, VNC)
        
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
    signature = c("character", "StQ"),
    function(UnitNames, Correspondence){
        
        
        DD <- getDD(Correspondence)
        
        output <- UnitToIDDDNames(UnitNames, DD)
        
        return(output)
        
    }
)
