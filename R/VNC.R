#' @title S3 class object constructor for the correspondence between variable names
#'
#' @description Definition of an S3 class named \linkS4class{VarNameCorresp} with the correspondence 
#' between variable names used by the different production unit for a given statistical variable and
#' variable names according to the key-value pair data model with qualifiers given by statistical 
#' metadata.
#'
#' The class \linkS4class{VarNameCorresp} comprises a \linkS4class{list} whose components are 
#' \linkS4class{data.table}s with a row per each variable and the following columns:
#'
#' \itemize{
#'  \item \code{IDQual}: Names of unit qualifiers.
#'  \item \code{NonIDQual}: Names of variable names qualifiers.
#'  \item \code{IDDD}: Names of the variables.
#'  \item \emph{One column per each name of the \code{IDQual} qualifiers}: 
#'  Values of the corresponding \code{IDQual} Qualifier for each variable (row).
#'  \item \emph{One column per each name of the \code{NonIDQual} qualifiers}: 
#'  Values of the corresponding \code{NonIDQual} Qualifier for each variable 
#'  (row).
#'  \item \code{UnitName}: Variable name used in production.
#'  \item \code{InFiles}: code of the files where each variable is included.
#' }
#'
#' @examples
#' library(data.table)
#' VNC <- VNC(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                            NonIDQual = c('','','','',''),
#'                            IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                            NumIdEst = c('', rep('.', 4)),
#'                            UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                            InFiles = rep('FI', 5)),
#'            MicroData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                   NonIDQual = c('', 'Market', ''),
#'                                   IDDD = c(rep('', 2), 'NewOrders'),
#'                                   NumIdEst = c(rep('', 2), '.'),
#'                                   Market = c(rep('', 2), '1'),
#'                                   UnitName = c('numidest', '', 'cp09'),
#'                                   InFiles = rep('FF, FD, FG', 3)),
#'            ParaData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                  NonIDQual = c('', 'Action', ''),
#'                                  IDDD = c(rep('', 2), 'Date'),
#'                                  NumIdEst = c(rep('', 2), '.'),
#'                                  Action = c(rep('', 2), 'Imputation'),
#'                                  UnitName = c('numidest', '', 'FechaImput'),
#'                                  InFiles = rep('FP', 3)))
#'
#' @import data.table 
#' 
#'
#' @export
VNC <- function(ID = data.table(IDQual = character(0),
                                NonIDQual = character(0),
                                IDDD = character(0),
                                UnitName = character(0),
                                InFiles = character(0)),
                MicroData = data.table(IDQual = character(0),
                                       NonIDQual = character(0),
                                       IDDD = character(0),
                                       UnitName = character(0),
                                       InFiles = character(0)),
                ...){
    
    
    object <- list(
        ID = ID,
        MicroData = MicroData,
        ...
    )

    OtherSlots <- list(...)
    ComponentNames <- c('ID', 'MicroData', names(OtherSlots))
    RootCompNames <- unlist(lapply(ComponentNames, function(Name){
        
        strsplit(Name, '_', fixed = TRUE)[[1]][1]
        
    }))
    RootCompNames <- unique(RootCompNames)
    ValidNames <- c('ID', 'MicroData', 'ParaData', 'Aggregates', 'AggWeights', 'Other')
    
    if (!all(RootCompNames %in% ValidNames)) {
        
        stop('[StQ::validity VarNameCorresp] The valid names must be ID, MicroData, ParaData, Aggregates, AggWeights, Other or be compound with underscores beginning with these names.\n')
        
    }
    
    if (!all(ValidNames[1:2] %in% RootCompNames)) {
        
        stop('[StQ::Validity VarNameCorresp] An S3 object VNC must be a named list with at least components with names ID and MicroData.\n')
        
    }     
    
    if (!all(unlist(lapply(object, inherits, 'data.table')))) {
        
        stop('[StQ::Validity VarNameCorresp] All components of the input list must be objects of class data.table.\n')     
        
    }
    ComponentNames <- lapply(names(object), function(CompName){

        DT <- object[[CompName]]
        if (dim(DT)[1] != 0) {
            
            DT <- DT[rowSums(DT == '') != ncol(DT)]
            object[[CompName]] <- DT
            
        }
        
        Empty <- TRUE
        for (col in names(DT)){
            
            
        }
        
        
        NCol <- dim(DT)[2]
        ColNames <- names(DT)
        
        if (ColNames[1] != 'IDQual') {
            
            stop(paste0('[StQ::validity VNC] The first column of component ', CompName, ' must be "IDQual".'))
        }
        if (ColNames[2] != 'NonIDQual') {
            
            stop(paste0('[StQ::validity VNC] The second column of component ', CompName, ' must be "NonIDQual".'))
        }
        if (ColNames[3] != 'IDDD') {
            
            stop(paste0('[StQ::validity VNC] The third column of component ', CompName, ' must be "IDDD".'))
        }
        
        IDQuals <- DT[['IDQual']]
        IDQuals <- IDQuals[IDQuals != '']
        if (any(duplicated(IDQuals))) {
            
            stop(paste0('[StQ::Validity VNC] The column IDQual of component ', CompName, ' cannot have repeated values.'))
            
        }
        IDQualCols <- intersect(ColNames, IDQuals)
        if (!all(IDQuals %in% IDQualCols)) {
            
            stop(paste0('[StQ::validity VNC] Every unit qualifier in column IDQual of component ', CompName, ' must appear also as a column in the same order.'))
            
        }
        
        NonIDQuals <- DT[['NonIDQual']]
        NonIDQuals <- NonIDQuals[NonIDQuals != '']
        if (any(duplicated(NonIDQuals))) {
            
            stop(paste0('[StQ::Validity VNC] The column "NonIDQual" of component ', CompName, ' cannot have repeated values.'))
            
        }
        NonIDQualCols <- intersect(ColNames, NonIDQuals)
        if (!all(NonIDQuals %in% NonIDQualCols)) {
            
            stop(paste0('[StQ::validity VNC] Every variable qualifier in column NonIDQual of component ', CompName, ' must appear also as a column in the same order.'))
            
        }
        QualsinRows <- c(IDQuals, NonIDQuals)
        QualsinCols <- setdiff(ColNames, c('IDQual', 'NonIDQual', 'IDDD', 'UnitName', 'InFiles'))
        if (!all(QualsinCols %in% QualsinRows)) {
            
            stop(paste0('[StQ::validity VNC] There exist qualifiers in the column names of component ', CompName, ' not included in its rows.'))
        }
        
        if (ColNames[length(ColNames) - 1] != 'UnitName') {
            
            stop(paste0('[StQ::validity VNC] The penultimate column of component ', CompName, ' must be "UnitName".'))
        }
        
        if (ColNames[length(ColNames)] != 'InFiles') {
            
            stop(paste0('[StQ::validity VNC] The last column of component ', CompName, ' must be "InFiles".'))
        }
        
        setkeyv(DT, setdiff(names(DT), c('UnitName', 'InFiles')))

        if (dim(DT[duplicated(DT, by = key(DT))])[1] != 0) stop('[StQ::validity VNC] Components cannot have repeated rows in VNC objects.\n')
        
        UnitName <- DT[['UnitName']]
        UnitName <- UnitName[UnitName != '']
        if (any(duplicated(UnitName)) != 0) stop('[StQ::validity VNC] UnitName cannot have repeated rows in VNC objects.\n')
        
        return(TRUE)
        
    })

    if (!all(unlist(ComponentNames))) stop('[StQ:: validity VNC] Check the names of the components of VarNameCorresp.')
    
    IDQuals <- c()
    NonIDQuals <- c()
    IDDDs <- c()
    for (CompName in names(object)){
        
        localObject <- object[[CompName]]
        auxIDQual <- unique(localObject[['IDQual']])
        auxIDQual <- auxIDQual[auxIDQual != '']
        IDQuals <- c(IDQuals, auxIDQual)
        
        auxNonIDQual <- unique(localObject[['NonIDQual']])
        auxNonIDQual <- auxNonIDQual[auxNonIDQual != '']
        NonIDQuals <- c(NonIDQuals, auxNonIDQual)
        
        auxIDDD <- unique(localObject[['IDDD']])
        auxIDDD <- auxIDDD[auxIDDD != '']
        IDDDs <- c(IDDDs, auxIDDD)
        
    }
    
    if (length(intersect(IDQuals, NonIDQuals)) != 0) stop('[StQ:: validity VNC] There cannot exist IDQuals and NonIDQuals with the same name.\n')
    if (length(intersect(IDQuals, IDDDs)) != 0) stop('[StQ:: validity VNC] There cannot exist IDQuals and IDDDs with the same name.\n')
    if (length(intersect(NonIDQuals, IDDDs)) != 0) stop('[StQ:: validity VNC] There cannot exist NonIDQuals and IDDDs with the same name.\n')
    
    
    
    class(object) <- append("VNC", class(object))
    return(object)

}

setOldClass(c('list'))
setOldClass(c('VNC', 'list'))
