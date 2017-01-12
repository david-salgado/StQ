#' @title Convert IDDD variable names into their corresponding production unit names
#'
#' @description \code{IDDDToUnitNames} returns a data table with the production unit variable name 
#' for each IDDD variable name.
#' 
#' @param IDDDNames Character vector with the IDDD variables.
#' 
#' @param Correspondence Object with the IDDD variable identifiers (of class \linkS4class{DD} or 
#' \linkS4class{StQ}).
#' 
#' @return \linkS4class{data.table} with the IDDD variable names and their corresponding production 
#' unit names.
#'
#' @examples
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = new(Class = 'VNCdt',
#'                 data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                            NonIDQual = c('','','','',''),
#'                            IDDD = c('', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                            NumIdEst = c('', rep('.', 4)),
#'                            UnitName = c('numidest', 'nombre', 'apellidos', 'direccion', 'telefono'),
#'                            InFiles = rep('FI', 5))),
#' MicroData = new(Class = 'VNCdt', data.table(IDQual = c('NumIdEst', rep('', 2)),
#'                                             NonIDQual = c('', 'Market', ''),
#'                                             IDDD = c(rep('', 2), 'NewOrders'),
#'                                             NumIdEst = c(rep('', 2), '.'),
#'                                             Market = c(rep('', 2), '1.'),
#'                                             UnitName = c('numidest', '', 'cp09'),
#'                                             InFiles = rep('FF, FD, FA', 3))),
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
#' IDDDToUnitNames('Date_Imputation', DD)
#'
#'
#' @export
setGeneric("IDDDToUnitNames", function(IDDDNames, Correspondence){standardGeneric("IDDDToUnitNames")})

#' @rdname IDDDToUnitNames
#'
#' @include DD-class.R VarNameCorresp-class.R DatadtToDT.R getVNC.R plus.VNCdt.R getDotQual.R getDoubleDotQual.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "IDDDToUnitNames",
    signature = c("character", "DD"),
    function(IDDDNames, Correspondence){
        
        if (missing(IDDDNames)) stop('[StQ::IDDDToUnitNames] A character vector of IDDDNames must be specified.\n')
        if (missing(Correspondence)) stop('[StQ::IDDDToUnitNames] A correspondence object (DD or StQ) must be specified.\n')
        IDDDNames_Orig <- IDDDNames
        IDDDNames <- setdiff(IDDDNames, getDotQual(Correspondence))
        Suffixes <- VarNamesToDT(IDDDNames, Correspondence)
        CommonDDQual <- intersect(names(Suffixes), getDoubleDotQual(Correspondence))
        Suffixes <- Suffixes[, c('IDDD', CommonDDQual), with = FALSE]
        # OJO: ESTAMOS SUPONIENDO QUE SOLO SE TIENE UN CALIFICADOR .. (LO CONTRARIO SERIA UNA 
        # LOCURA PARA LLAMAR A LAS VARIABLES; ESTAMOS POR TANTO ESTE COMPORTAMIENTO EN LA 
        # CONSTRUCCION DE NOMBRES DE VARIABLES)
        Suffixes[, Suffix := '']
        colSuffixes <- names(Suffixes)
        for (col in setdiff(colSuffixes, 'IDDD')){
            
            Suffixes[, Suffix := ifelse(Suffix == '', get(col), Suffix)]
            
        }
        Suffixes <- Suffixes[, c('IDDD', 'Suffix'), with = F] 
        
        VNC <- getVNC(Correspondence)
        XLS <- Reduce(`+`, VNC, init = VNC[[1]])
        XLS <- DatadtToDT(XLS)
        XLS[, IDDDName := '']
        
        DDslots <- setdiff(slotNames(Correspondence), 'VarNameCorresp')
        DDdt <- new(Class = 'DDdt')
        for (DDslot in DDslots){
            
            DDdt <- DDdt + slot(Correspondence, DDslot)
            
        }
        RootNames <- unique(ExtractNames(IDDDNames_Orig))
        DotQual <- getDotQual(Correspondence)

        Quals.list <- lapply(RootNames, function(IDDDName){
            
            QualsDT <- DDdt[Variable == IDDDName, names(DDdt)[grep('Qual', names(DDdt))], with = F]
            Quals <- t(QualsDT)[, 1]
            Quals <- Quals[Quals != '']
            Quals <- Quals[!Quals %in% DotQual]
            return(Quals)
        })
        
        names(Quals.list) <- RootNames

        XLS <- XLS[IDDD %in% RootNames | IDQual %in% RootNames | NonIDQual %in% RootNames]
        
        NotBlankIDDDNames <- XLS[['IDDD']]
        NotBlankIDDDNames <- unique(NotBlankIDDDNames[NotBlankIDDDNames != ''])
        IDQuals <- getIDQual(Correspondence)
        NonIDQuals <- getNonIDQual(Correspondence)
        UnitNames <- lapply(NotBlankIDDDNames, function(IDDDname){
            
            localXLS <- XLS[IDDD == IDDDname & IDDD != '']
            QualNames <- Quals.list[[IDDDname]]
            localXLS <- localXLS[, c('IDDD', QualNames, 'UnitName', 'IDDDName'), with = F]
            localNonIDQuals <- setdiff(intersect(names(localXLS), NonIDQuals), setdiff(IDQuals, NonIDQuals))

            for (col in localNonIDQuals) {
                
                #if (any(localXLS[[col]] == '.')) next
                localXLS[, IDDDName := paste(IDDDName, get(col), sep = '_')]
                
            }
            localXLS[, IDDDName := paste0(IDDD, IDDDName)]
            localXLS <- localXLS[, c('UnitName', 'IDDDName'), with = F]
            #localXLS <- localXLS[IDDDName %in% IDDDNames | ExtractNames(IDDDName) %in% IDDDNames][, c('UnitName', 'IDDDName'), with = F]
            #IDDD_aux <- localXLS[grep('\\.\\.', localXLS[['IDDDName']]), 'IDDDName']
            #IDDD_aux <- strsplit(t(IDDD_aux), '\\.\\.')
            #IDDD_aux <- unlist(lapply(IDDD_aux, function(x){x[1]}))
            #IDDD_aux <- unlist(lapply(IDDD_aux, function(name){localXLS[grep(name, localXLS[['IDDDName']]), 'IDDDName'][['IDDDName']]}))
            #localXLS <- localXLS[IDDDName %in% IDDDNames | ExtractNames(IDDDName) %in% IDDDNames| IDDDName %in% IDDD_aux][, c('UnitName', 'IDDDName'), with = F]
            return(localXLS)
            
        })
        names(UnitNames) <- NotBlankIDDDNames

        if (any(Suffixes[['Suffix']] != '')){
            
            outList <- lapply(1:(dim(Suffixes)[1]), function(IDDDNamesRoot.index){
                        IDDDNamesRoot <- Suffixes[IDDDNamesRoot.index][['IDDD']]
                        Suffix <- Suffixes[IDDDNamesRoot.index][['Suffix']]
                        out <- copy(UnitNames[[IDDDNamesRoot]])
                        out[, IDDDName := gsub('..', Suffix, IDDDName, fixed = TRUE)]
                        out[, UnitName := gsub('\\[.+\\]', Suffix, UnitName)]
                        out <- out[IDDDName %in% IDDDNames | ExtractNames(IDDDName) %in% IDDDNames]
                        return(out)
            })
            
        } else {
            
            outList <- UnitNames
            
        }

        UnitNames <- rbindlist(outList)
        
        IDQualXLS <- XLS[IDQual != '']
        IDQualXLS[, IDDDName := IDQual]
        IDQualXLS <- IDQualXLS[, c('UnitName', 'IDDDName'), with = F]
        
        NonIDQualXLS <- XLS[NonIDQual != '']
        NonIDQualXLS[, IDDDName := NonIDQual]
        NonIDQualXLS <- NonIDQualXLS[, c('UnitName', 'IDDDName'), with = F]
        
        UnitNames <- rbindlist(list(IDQualXLS, NonIDQualXLS, UnitNames))
        IDDDNamesDT <- data.table(IDDDName = IDDDNames_Orig)
        outDT <- merge(UnitNames, IDDDNamesDT, by = 'IDDDName', all.y = TRUE)
        out <- outDT[['UnitName']]
        names(out) <- outDT[['IDDDName']]
        out <- out[IDDDNames_Orig]
        return(out)
    }
)

#' @rdname IDDDToUnitNames
#'
#' @include StQ-class.R DD-class.R getDD.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "IDDDToUnitNames",
    signature = c("character", "StQ"),
    function(IDDDNames, Correspondence){
        
        
        DD <- getDD(Correspondence)
        
        output <- IDDDToUnitNames(IDDDNames, DD)
        
        return(output)
        
    }
)
