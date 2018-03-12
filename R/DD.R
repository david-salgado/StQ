#' @title S3 class with the data dictionary (variable specifications)
#'
#' @description Definition of an S3 class named \code{DD} with the specification of each variable.
#'
#' The class \code{DD} comprises a \link{list} whose first component is an object of class 
#' \linkS4class{VarNameCorresp} and name \code{VNC} and the rest of components are objects of class 
#' \linkS4class{data.table} of names \code{ID}, \code{MicroData}, \code{ParaData}, 
#' \code{Aggregates}, \code{AggWeights}, \code{Other}.
#'
#' @examples
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                 NonIDQual = c('','','','',''),
#'                                 IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                                 NumIdEst = c('', rep('.', 4)),
#'                                 UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                                 InFiles = rep('FI', 5)),
#' MicroData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                        NonIDQual = c('', 'Market', ''),
#'                        IDDD = c(rep('', 2), 'NewOrders'),
#'                        NumIdEst = c(rep('', 2), '.'),
#'                        Market = c(rep('', 2), '1'),
#'                        UnitName = c('numidest', '', 'cp09'),
#'                        InFiles = rep('FF, FD, FG', 3)),
#' ParaData = data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                       NonIDQual = c('', 'Action', ''),
#'                       IDDD = c(rep('', 2), 'Date'),
#'                       NumIdEst = c(rep('', 2), '.'),
#'                       Action = c(rep('', 2), 'Imputation'),
#'                       UnitName = c('numidest', '', 'FechaImput'),
#'                       InFiles = rep('FP', 3)))
#'
#' VNC <- BuildVNC(VarList)
#'
#' IDdt <- data.table(Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                    Sort = c('IDQual', rep('IDDD', 4)),
#'                    Class = rep('character', 5),
#'                    Length = c('11', '15', '15', '20','9'),
#'                    Qual1 = c('', rep('NumIdEst', 4)),
#'                    ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}'))
#' Microdt <- data.table(Variable = c('NumIdEst', 'Market', 'NewOrders'),
#'                       Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                       Class = c(rep('character', 2), 'numeric'),
#'                       Length = c('11', '2', '7'),
#'                       Qual1 = c(rep('', 2), 'NumIdEst'),
#'                       ValueRegExp = c('[0-9]{9}PP', '.+', '([0-9]{1, 10}| )'))
#' Paradt <- data.table(Variable = c('NumIdEst', 'Action', 'Date'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = rep('character', 3),
#'                      Length = c('11', '10', '10'),
#'                      Qual1 = c(rep('', 2), 'NumIdEst'),
#'                      Qual2 = c(rep('', 2), 'Action'),
#'                      ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation',
#'                                      '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )'))
#'
#' DD <- DD(VNC = VNC, ID = IDdt, MicroData = Microdt, ParaData = Paradt)
#' DD
#' 
#' @include  VNC.R BuildVNC.R
#'
#' @import data.table
#'
#' @export
DD <- function(VNC = BuildVNC(),
               ID = data.table(Variable = character(0),
                               Sort = character(0),
                               Class = character(0),
                               Length = character(0),
                               Qual1 = character(0),
                               ValueRegExp = character(0)),
               MicroData = data.table(Variable = character(0),
                                      Sort = character(0),
                                      Class = character(0),
                                      Length = character(0),
                                      Qual1 = character(0),
                                      ValueRegExp = character(0)),
               ParaData = data.table(Variable = character(0),
                                     Sort = character(0),
                                     Class = character(0),
                                     Length = character(0),
                                     Qual1 = character(0),
                                     ValueRegExp = character(0)),
               Aggregates = data.table(Variable = character(0),
                                       Sort = character(0),
                                       Class = character(0),
                                       Length = character(0),
                                       Qual1 = character(0),
                                       ValueRegExp = character(0)),
               AggWeights = data.table(Variable = character(0),
                                       Sort = character(0),
                                       Class = character(0),
                                       Length = character(0),
                                       Qual1 = character(0),
                                       ValueRegExp = character(0)),
               Other = data.table(Variable = character(0),
                                  Sort = character(0),
                                  Class = character(0),
                                  Length = character(0),
                                  Qual1 = character(0),
                                  ValueRegExp = character(0))){
            
    object <- list(
        VNC = VNC,
        ID = ID,
        MicroData = MicroData,
        ParaData = ParaData,
        Aggregates = Aggregates,
        AggWeights = AggWeights,
        Other = Other
    )
    ColNames <- names(object)
    
    if (length(ColNames) > 7) {
        
        stop('[StQ:: validity DD] The names of the component of a DD object must be VNC, ID, MicroData, ParaData, Aggregates, AggWeights, Other.\n')
    }
    
    if (ColNames[1] != 'VNC') {

        stop('[StQ:: validity DD] The first component of DD must be VNC')
    }

    if (ColNames[2] != 'ID') {

        stop('[StQ:: validity DD] The second component of DD must be ID.')
    }

    if (ColNames[3] != 'MicroData') {

        stop('[StQ:: validity DD] The third component of DD must be MicroData.')
    }

    if (ColNames[4] != 'ParaData') {

        stop('[StQ:: validity DD] The fourth component of DD must be ParaData.')
    }

    if (ColNames[5] != 'Aggregates') {

        stop('[StQ:: validity DD] The fifth component of DD must be Aggregates.')
    }

    if (ColNames[6] != 'AggWeights') {

        stop('[StQ:: validity DD] The last but one component of DD must be AggWeights.')
    }

    if (ColNames[7] != 'Other') {

        stop('[StQ:: validity DD] The last component of DD must be Other.')
    }
            
    DDdtNames <- setdiff(ColNames, 'VNC')
    IDQuals <- c()
    NonIDQuals <- c()
    IDDDs <- c()
    ValidColNames <- lapply(DDdtNames, function(DDdtName){
                 
        localObject <- object[[DDdtName]]
        NCol <- dim(localObject)[2]
        if (NCol < 6) {
                     
            stop(paste0('[StQ:: Validity DDdt] The component ', DDdtName, ' must be a data.table with at least five columns named "Variable", "Sort", "Class", "Length", "Qual1" and "ValueRegExp".\n'))   
        } 
                 
        if (!all(unlist(lapply(localObject, function(x){is.character(unlist(x)) }))) == TRUE) {
                     
                     stop(paste0('[StQ:: Validity DD] All columns of the component ', DDdtName, ' must be character vectors.\n'))
                     
        }
        setkeyv(localObject, 'Variable')
        if (sum(duplicated(localObject, by = key(localObject))) > 0) {
                     
            stop(paste0('[StQ:: Validity DD] No duplicated variable in component ', DDdtName, ' is allowed.\n'))
                     
        }
        NQual <- max(0, NCol - 6)
        if (NQual > 0) {
                     
            Quals <- paste0('Qual', 1:(NQual + 1))
                     
        } else {
                     
            Quals <- 'Qual1'
                     
        }
        LocalColNames <- c('Variable', 'Sort', 'Class', 'Length', 'ValueRegExp', Quals)
                 
        if (!all(names(localObject) %in% LocalColNames)) {
                     
            stop(paste0('[StQ:: Validity DD] The names of the component ', DDdtName, ' must be: Variable, Sort, Class, Length, Qual1-Qualj, ValueRegExp.\n'))
                     
        }
                 
        DTColNames <- names(localObject)
        if (DTColNames[1] != 'Variable') {
                     
            stop(paste0('[StQ:: Validity DD] The first column of the component ', DDdtName, ' must be "Variable".\n'))   
        }
                 
        if (any(duplicated(localObject[['Variable']]))) {
                     
            stop(paste0('[StQ:: Validity DD] The column "Variable" of the component ', DDdtName, ' cannot have repeated values.\n'))
        }
                 
        if (DTColNames[2] != 'Sort') {
                     
            stop(paste0('[StQ:: Validity DD] The second column of component ', DDdtName, ' must be "Sort".\n'))
        }
                 
        if (length(localObject[['Sort']]) != 0 &&  !all(localObject[['Sort']] %in% c('IDQual', 'NonIDQual', 'IDDD'))) { 
                     
            stop(paste0('[StQ:: Validity DD] The column "Sort" of the component ', DDdtName, ' can only have values "IDQual", "NonIDQual" and "IDDD".\n'))
        }
                 
        if (DTColNames[3] != 'Class') {
                     
            stop(paste0('[StQ:: Validity DD] The third column of the component ', DDdtName, ' must be named "Class".\n'))
        }
                 
        if (DTColNames[4] != 'Length') {
                     
            stop(paste0('[StQ:: Validity DD] The fourth column of the component ', DDdtName, ' must be named "Length".\n'))
                     
        }
                 
        if (unique(substr(DTColNames[5:(4 + length(Quals))], 1, 4)) != 'Qual') {
                     
            stop(paste0('[StQ:: Validity DD] The fifth and next (if any) columns of the component ', DDdtName, ' must be named "Qualj".\n'))
        }
                 
                 
        if (DTColNames[length(DTColNames)] != 'ValueRegExp') {
                     
            stop(paste0('[StQ:: Validity DD] The last column of of the component ', DDdtName, ' must be named "ValueRegExp".\n'))
        }
        
        return(TRUE)
                 
    }) 
    if (!all(unlist(ValidColNames))) stop('[StQ:: Validity DD] Check the names of the components.')
    
    IDQuals <- c()
    NonIDQuals <- c()
    IDDDs <- c()
    for (DDdtName in DDdtNames) {
        
        localObject <- object[[DDdtName]]
        auxIDQual <- unique(localObject[Sort == 'IDQual'][['Variable']])
        auxIDQual <- auxIDQual[auxIDQual != '']
        IDQuals <- c(IDQuals, auxIDQual)
        
        auxNonIDQual <- unique(localObject[Sort == 'NonIDQual'][['Variable']])
        auxNonIDQual <- auxNonIDQual[auxNonIDQual != '']
        NonIDQuals <- c(NonIDQuals, auxNonIDQual)
        
        auxIDDD <- unique(localObject[Sort == 'IDDD'][['Variable']])
        auxIDDD <- auxIDDD[auxIDDD != '']
        IDDDs <- c(IDDDs, auxIDDD)
        
    }

    if (length(intersect(IDQuals, NonIDQuals)) != 0) stop('[StQ:: validity DD] There cannot exist IDQuals and NonIDQuals with the same name.\n')
    if (length(intersect(IDQuals, IDDDs)) != 0) stop('[StQ:: validity DD] There cannot exist IDQuals and IDDDs with the same name.\n')
    if (length(intersect(NonIDQuals, IDDDs)) != 0) stop('[StQ:: validity DD] There cannot exist NonIDQuals and IDDDs with the same name.\n')
    
    
             
    variablesDD <- c()
    for (Slot in DDdtNames) {
                 
        SlotNames <- object[[Slot]]
        variablesDD <- c(variablesDD,  SlotNames$Variable[SlotNames$Sort == 'IDDD'])
        variablesDD <- unique(variablesDD)
    }
      
    variablesVNC <- c()
    for (Component in object[['VNC']]) {
      
      auxVar <- Component[['IDDD']]
      auxVar <- auxVar[auxVar != '']
      variablesVNC <- c(variablesVNC, auxVar)
    }
  
    if (length(variablesDD) > 0 & length(variablesVNC) == 0) stop('[StQ:: validity DD] The component VNC must be specified with the variables in the other components')
    varDDnotinVNC <- setdiff(variablesDD, variablesVNC)
    if (length(varDDnotinVNC) > 0) {
      
      stop(paste0('[StQ:: Validity DD] The following variables in a column "IDDD" of the slot DD must be variables (IDDD) in the slot VNC:\n',
                  paste0(varDDnotinVNC, collapse = ', '),
                  '\n\n Check if object VNC contains all variable names.'))
      
    }
    

    class(object) <- append("DD", class(object))
    return(object)
    
}

setOldClass(c('list'))
setOldClass(c('DD', 'list'))
