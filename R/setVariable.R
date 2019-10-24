#' @title Generate a new variable
#'
#' @description \code{setVariable} .
#'
#' @param object Object of class \linkS4class{StQ} or \linkS4class{StQList}.
#' 
#' @param Var Object of class \linkS4class{Variable}.
#'
#' @return Input object including the new variable.
#'
#' @examples
#' data(ExampleStQ)
#' Var <- new(Class = 'Variable', UnitName = 'LRTEmp',
#'                                IDDD = 'Employees',
#'                                QualsValues = list(ID = '', EmplType = ''),
#'                                Length = '8',
#'                                ClassVar = 'numeric',
#'                                ValueRegExp = '[.]+',
#'                                Formula = as.call(list('log( 1 + (Turnover / (Employees_1. + Employees_2.1.)))')),
#'                                SlotName = 'MicroData',
#'                                Literal = '',
#'                                DDversion = '1')
#' ExampleStQ <- setVariable(ExampleStQ, Var)
#' 
#' ExampleStQList <- BuildStQList(list(MM012012 = ExampleStQ, MM022012 = ExampleStQ))
#' ExampleStQList <- setVariable(ExampleStQList, Var)
#' 
#' @include VNC.R DD.R StQ.R getDD.R getVNC.R getMicroData.R getIDDD.R BuildVNC.R  BuildDD.R setValues.R
#' 
#' @import data.table 
#'
#' 
#' @export
setGeneric("setVariable",
           function(object, Var, lag = NULL, by = NULL) {standardGeneric("setVariable")})

#' @rdname setVariable
#' 
#' @include VNC.R DD.R StQ.R getDD.R getVNC.R getMicroData.R getIDDD.R BuildVNC.R  BuildDD.R setValues.R
#' 
#' @import data.table 
#' 
#' @export
setMethod(
    f = "setVariable",
    signature = c("StQ", "Variable"),
    function(object, Var, lag = NULL, by = NULL) {
        
        UnitName <- Var@UnitName
        IDDD <- Var@IDDD
        QualsValues <- Var@QualsValues
        
        DD <- getDD(object)
        if (IDDD %in% getIDDD(object)){
            
            DD.aux <- DD
            DD.aux[['VNC']] <- NULL
            DD.aux <- rbindlist(DD.aux, fill = TRUE)
            DD.aux <- DD.aux[Variable == IDDD][1]
            Quals <- DD.aux[, names(DD.aux)[grep('Qual', names(DD.aux))], with = FALSE]
            Quals <- as.vector(t(Quals))
            Quals <- Quals[Quals != '']
            if (!identical(Quals, names(QualsValues))) {
                
                stop(paste0('[setVariable validation] The names of slot QualsValues in Var must be: ', paste0(Quals, collapse = ','), ' and in this order. If any  qualifier is not necessary, its value must be \'\' .'))
            }
            setcolorder(QualsValues, Quals)
        }
        
        newVNC <- getMicroData(getVNC(DD))[IDQual != '' | NonIDQual != '']
        newVNCVar <- data.table(matrix(ncol = length(names(newVNC)), nrow = 1))
        newVNCVar[is.na(newVNCVar)] <- ''
        setnames(newVNCVar, names(newVNCVar), names(newVNC))
        values <- c(list(IDDD = IDDD, UnitName = UnitName), QualsValues)
        newVNCVar[, (names(values)) := values]
        newVNC <- rbind(newVNC, newVNCVar)
        newVNC <- BuildVNC(list(MicroData = newVNC))
        
        
        newDD <- getMicroData(DD)[Sort %in% c('IDQual', 'NonIDQual')]
        newDDVar <- data.table(matrix(ncol = length(names(newDD)), nrow = 1))
        newDDVar[is.na(newDDVar)] <- ''
        setnames(newDDVar, names(newDDVar), names(newDD))
        Quals <- as.list(names(QualsValues))
        names(Quals) <- paste0('Qual', 1:length(QualsValues))
        Vars <- c(list(Variable = IDDD, Sort = 'IDDD', Class = Var@ClassVar, Length = Var@Length, ValueRegExp = Var@ValueRegExp), Quals)
        newDDVar[, (names(Vars)) := Vars]
        newDD <- rbind(newDDVar, newDD)
        newDD <- BuildDD(list(VNC = newVNC, MicroData = newDD))
        
        output <- setValues(object = object,
                            newDD = newDD,
                            DDslot = Var@SlotName,
                            Value = Var@Formula,
                            lag = lag,
                            by = by)
        return(output)
        
    }
)

#' @rdname setVariable
#'
#' @include VNC.R DD.R StQ.R getDD.R getVNC.R getMicroData.R getIDDD.R BuildVNC.R  BuildDD.R setValues.R
#' 
#' @import data.table 
#' 
#' @export
setMethod(
    f = "setVariable",
    signature = c("StQList", "Variable"),
    function(object, Var, lag = NULL, by = NULL) {
        
        # UnitName <- Var@UnitName
        # IDDD <- Var@IDDD
        # QualsValues <- Var@QualsValues
        # 
        # nPeriods <- length(getPeriods(object))
        # DD <- getDD(object[[nPeriods]])
        # if (IDDD %in% getIDDD(object[[nPeriods]])){
        #   
        #   DD.aux <- DD
        #   DD.aux[['VNC']] <- NULL
        #   DD.aux <- rbindlist(DD.aux, fill = TRUE)
        #   DD.aux <- DD.aux[Variable == IDDD]
        #   Quals <- DD.aux[, names(DD.aux)[grep('Qual', names(DD.aux))], with = FALSE]
        #   Quals <- as.vector(t(Quals))
        #   Quals <- Quals[Quals != '']
        #   if (length(setdiff(Quals, names(QualsValues))) > 0) {
        #     
        #     stop(paste0('[setVariable validation] The names of slot QualsValues in Var must be: ', paste0(Quals, collapse = ','), '. If any  qualifier is not necessary, its value must be \'\' .'))
        #   }
        # }
        # 
        # newVNC <- getMicroData(getVNC(DD))[IDQual != '' | NonIDQual != '']
        # newVNCVar <- data.table(matrix(ncol = length(names(newVNC)), nrow = 1))
        # newVNCVar[is.na(newVNCVar)] <- ''
        # setnames(newVNCVar, names(newVNCVar), names(newVNC))
        # values <- c(list(IDDD = IDDD, UnitName = UnitName), QualsValues)
        # newVNCVar[, (names(values)) := values]
        # newVNC <- rbind(newVNC, newVNCVar)
        # newVNC <- BuildVNC(list(MicroData = newVNC))
        # 
        # 
        # newDD <- getMicroData(DD)[Sort %in% c('IDQual', 'NonIDQual')]
        # newDDVar <- data.table(matrix(ncol = length(names(newDD)), nrow = 1))
        # newDDVar[is.na(newDDVar)] <- ''
        # setnames(newDDVar, names(newDDVar), names(newDD))
        # Quals <- as.list(names(QualsValues))
        # names(Quals) <- paste0('Qual', 1:length(QualsValues))
        # Vars <- c(list(Variable = IDDD, Sort = 'IDDD', Class = Var@ClassVar, Length = Var@Length, ValueRegExp = Var@ValueRegExp), Quals)
        # newDDVar[, (names(Vars)) := Vars]
        # newDD <- rbind(newDDVar, newDD)
        # newDD <- BuildDD(list(VNC = newVNC, MicroData = newDD))
        # 
        # output <- setValues(object = object,
        #                     newDD = newDD,
        #                     DDslot = Var@SlotName,
        #                     Value = Var@Formula,
        #                     lag = lag,
        #                     by = by)
        
        
        Object.List <- getData(object)
        mc <- match.call()
        
        output <- lapply(seq(along = Object.List), function(i) {
            
            cat(paste0('  [StQ::setVariable] Setting values for time period ', names(Object.List)[i], '... '))
            Localmc <- mc
            
            if (!is.null(lag)) {
                
                if (i <= lag) {
                    
                    LocalOutput <- Object.List[[i]]
                    
                } else {
                    
                    Value <- Var@Formula
                    QualsValues <- Var@QualsValues
                    
                    LocalOutput.init <- Object.List[[i]]
                    IDQuals <- getIDQual(LocalOutput.init)
                    QualsValues <- QualsValues[!names(QualsValues) %in% IDQuals]
                    
                    Localmc[['object']] <- Object.List[[(i - lag)]]
                    Var@Formula <- getValues(Object.List[[(i - lag)]], Value)[[Value]]
                    Localmc[['Var']] <- Var
                    LocalOutputAux <- eval(Localmc)
                    DD.Aux <- getDD(LocalOutputAux)
                    setDD(LocalOutput.init) <- DD.Aux
                    
                    LocalOutputAux <- LocalOutputAux[IDDD == Var@IDDD]
                    cols <- names(getData(LocalOutputAux))
                    Quals <- intersect(cols, names(QualsValues))
                    setcolorder(QualsValues, Quals)
                    VarName <- paste0(QualsValues, collapse = '_')
                    VarName <- paste(Var@IDDD, VarName, sep = '_')
                    LocalOutputAux.Data <- dcast_StQ(LocalOutputAux)[, c(intersect(cols, IDQuals), VarName), with = FALSE]
                    LocalOutputAux <- melt_StQ(LocalOutputAux.Data, DD.Aux)
                    LocalOutput <- LocalOutput.init + LocalOutputAux
                    
                }
                
            } else {
                
                Localmc[['object']] <- Object.List[[i]]
                LocalOutput <- eval(Localmc)
            }
            cat(' ok.\n')
            return(LocalOutput)
            
        })
        
        if (!is.null(lag)) {
            
            newDD <- getDD(output[[length(Object.List)]])
            for (i in seq(1, lag)) {
                
                LocalOutput <- output[[i]]
                setDD(LocalOutput) <- newDD
                output[[i]] <- LocalOutput
            }
        }
        
        cat('  [StQ::setVariable] Building StQList object ...')
        names(output) <- names(Object.List)
        output <- BuildStQList(output)
        cat('   ok.\n')
        return(output)
        
    }
)
