#' @title Convert production unit names into their corresponding statistical variable names (IDDD)
#'
#' @description \code{UnitToIDDDNames} returns a \linkS4class{data.table} with the statistical
#' variable name (IDDD + Qualifiers) corresponding to the production unit variable name specified as
#' input argument.
#'
#' @param UnitNames \code{Character} vector with the production unit variable name.
#'
#' @param Correspondence Object with the IDDD variable name.
#'
#' @return Returns a \code{character} vector with all the corresponding IDDD variable names. For
#' objects the classes \link{DD} and \link{StQ} it returns the IDDD in the slot 
#' VarNameCorresp of the corresponding DD object.
#'
#' @details IDDD and qualifiers compose together the so-called IDDDname of the variable by pasting
#' the IDDD identifier and each consecutive qualifier with an underscore _.
#'
#' @examples
#' # An example for VNC and DD objects:
#' library(data.table)
#' ### We build the VNC object
#' VarList <- list(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
#'                                             NonIDQual = c(rep('',5)),
#'                                             IDDD = c('', 'Name', 'Surname', 'PostalAddr',
#'                                                      'PhoneNo'),
#'                                             NumIdEst = c('', rep('.', 4)),
#'                                             UnitName = c('numidest', 'nombre', 'apellidos',
#'                                                       'direccion', 'telefono'),
#'                                             InFiles = rep('FF', 5)),
#'                 MicroData = data.table(
#'                                         IDQual = c('NumIdEst', rep('', 2)),
#'                                         NonIDQual = c('', 'Market', ''),
#'                                         IDDD = c(rep('', 2), 'NewOrders'),
#'                                         NumIdEst = c(rep('', 2), '.'),
#'                                         Market = c(rep('', 2), '2.'),
#'                                         UnitName = c('numidest', '', 'cp09'),
#'                                         InFiles = rep('FF', 3)),
#'                 ParaData = data.table(
#'                                         IDQual = c('NumIdEst', rep('', 2)),
#'                                         NonIDQual = c('', 'Action', ''),
#'                                         IDDD = c(rep('', 2), 'Date'),
#'                                         NumIdEst = c(rep('', 2), '.'),
#'                                         Action = c(rep('', 2), 'Imputation'),
#'                                         UnitName = c('numidest', '', 'FechaImput'),
#'                                         InFiles = rep('FP', 3)),
#'                 Aggregates = data.table(
#'                                          IDQual = c('Province', 'NACE09', '', ''),
#'                                          NonIDQual = c(rep('', 2), 'Market', ''),
#'                                          IDDD = c('', '', '', 'Turnover'),
#'                                          Province = c('', '', '', '.'),
#'                                          NACE09 = c('', '', '', '.'),
#'                                          Market = c('', '', '', '3.'),
#'                                          UnitName = c('provincia', 'actividad', '', 'cn01'),
#'                                          InFiles = rep('FA', 4)))
#'
#' VNC <- BuildVNC(VarList)
#'
#' ### We build the specification data.tables
#' IDdt <- data.table(
#'                      Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                      Sort = c('IDQual', rep('IDDD', 4)),
#'                      Class = rep('character', 5),
#'                      Length = c('11', '25', '25', '50', '9'),
#'                      Qual1 = c('', rep('NumIdEst', 4)),
#'                      ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}'))
#' Microdt <- data.table(
#'                      Variable = c('NumIdEst', 'Market', 'NewOrders'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = c(rep('character', 2), 'numeric'),
#'                      Length = c('11', '2', '7'),
#'                      Qual1 = c(rep('', 2), 'NumIdEst'),
#'                      Qual2 = c(rep('', 2), 'Market'),
#'                      ValueRegExp = c('[0-9]{9}PP', '(0|1| )', '([0-9]{1, 10}| )'))
#' Paradt <- data.table(
#'                      Variable = c('NumIdEst', 'Action', 'Date'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = rep('character', 3),
#'                      Length = c('11', '4', '10'),
#'                      Qual1 = c(rep('', 2), 'NumIdEst'),
#'                      Qual2 = c(rep('', 2), 'Action'),
#'                      ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation',
#'                      '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )'))
#' Aggdt <- data.table(
#'                      Variable = c('Province', 'NACE09', 'Turnover'),
#'                      Sort = c(rep('IDQual', 2), 'IDDD'),
#'                      Class = c(rep('character', 2), 'numeric'),
#'                      Length = c('25', '4', '12'),
#'                      Qual1 = c(rep('', 2), 'Province'),
#'                      Qual2 = c(rep('', 2), 'NACE09'),
#'                      ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )'))
#'
#' DD <- DD(VNC = VNC,
#'           ID = IDdt,
#'           MicroData = Microdt,
#'           ParaData = Paradt,
#'           Aggregates = Aggdt)
#'
#' UnitToIDDDNames(DD, UnitNames = c('cn01', 'cp09'))
#'
#' # An example for StQ objects:
#' data(ExampleStQ)
#' UnitToIDDDNames(ExampleStQ, UnitNames = c('cnae09','C11','C121','C122' , 'EXISTENCIAS', 'B1'))
#' 
#'
#' @include getIDQual.R VNC.R DD.R BuildVNC.R BuildDD.R StQ.R getDD.R
#' 
#' @export
setGeneric("UnitToIDDDNames", function(UnitNames, Correspondence){standardGeneric("UnitToIDDDNames")})

#' @rdname UnitToIDDDNames
#' 
#' @include DD.R VarNamesToDD.R getVNC.R BuildDD.R BuildVNC.R VNC.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "UnitToIDDDNames",
    signature = c("character", "DD"),
    function(UnitNames, Correspondence){

        IDQualsGlobal <- getIDQual(Correspondence)
        DD <- Correspondence
        Correspondence <- getVNC(DD)
        output.list <- lapply(names(Correspondence), function(nameVNC){

            VNC <- Correspondence[[nameVNC]]
            nameVNC <- ExtractNames(nameVNC)
            XLS <- VNC[UnitName %in% UnitNames]
            XLS[, IDDDName := IDDD]
            XLS.Quals <- XLS[IDDD == '']
            XLS.Quals[IDQual != '', IDDDName := IDQual]
            XLS.Quals[NonIDQual != '', IDDDName := NonIDQual]
            XLS.Quals <- XLS.Quals[, c('IDQual', 'NonIDQual', 'UnitName', 'IDDDName', 'InFiles'),
                                   with = F]
            IDQual <- XLS.Quals[IDQual != '']
            IDQual <- IDQual[IDQual != '']
            DotQual <- getDotQual(Correspondence)

            XLS <- XLS[IDDD != '']
            XLS <- XLS[, setdiff(names(XLS), IDQual), with = F]
            XLS.list <- split(XLS, XLS[['IDDD']])

            XLS.list <- lapply(XLS.list, function(xls){

                #ColNames <- setdiff(names(xls), IDQualsGlobal)
                #NotEmptyCols <- c()
                #for (col in ColNames){

                #    if (!all(is.na(xls[[col]]) | xls[[col]] == '')) NotEmptyCols <- c(NotEmptyCols, col)
                #
                #}

                #xls <- xls[, NotEmptyCols, with = F]

                #ColsNotUnit <- setdiff(names(xls), c('IDDD', 'UnitName', 'IDDDName', 'InFiles'))
                #ColsNotUnit <- intersect(names(VNC), ColsNotUnit)
                auxDT <- DD[[nameVNC]][Variable == unique(xls[['IDDDName']])]
                ColsNotUnit <- t(as.matrix(auxDT[, names(auxDT)[grep('Qual', names(auxDT))], with = FALSE]))[,1]
                ColsNotUnit <- setdiff(ColsNotUnit, c(IDQual, DotQual))
                ColsNotUnit <- ColsNotUnit[ColsNotUnit != '']
                
                for (col in ColsNotUnit) {

                    #if (all(xls[[col]] == '.') | all(is.na(xls[[col]]))) next
                    if (any(xls[[col]] == '.')) next
                    xls[, IDDDName := paste(IDDDName, get(col), sep = '_')]

                }
                return(xls)
            })

            output <- rbindlist(XLS.list, fill = TRUE)
            output <- rbindlist(list(output, XLS.Quals), fill = TRUE)

            aux <- output[, c('UnitName', 'IDDDName'), with = FALSE]

            # Patterns in UnitNames : [mm], [aa], [aaaa], [n], etc.
            UnitNames_aux <- unique(aux[['UnitName']])
            patrones <- UnitNames_aux[grep('[[]', UnitNames_aux)]
            UnitToIDDDNames.local <- function(UnitNamesLocal){

                outputNewName <- UnitNamesLocal[!UnitNamesLocal %in% output[['UnitName']]]


                if (length(outputNewName) > 0 & length(patrones) > 0){

                    metaVar <- lapply(patrones, function(patron){

                        patron_aux <- patron
                        patron <- gsub('\\[mm\\]', '(([0][1-9])|([1][0-2]))', patron)
                        patron <- gsub('\\[aa\\]', '[0-9]{2}', patron)
                        patron <- gsub('\\[aaaa\\]', '[0-9]{4}', patron)
                        patron <- gsub('\\[n\\]', '[0-9]+', patron)
                        patron <- gsub('\\[varGestion\\]', '.*', patron)
                        patron <- gsub('\\[ccaa\\]', '[0-9]{2}', patron)
                        patron <- gsub('\\[IDEdit\\]', '[.]+', patron)
                        Var <- lapply(outputNewName, function(name){
                            out <- regexpr(patron, name)
                            out <- regmatches(name, out)
                            names(out) <- rep(aux[UnitName %in% patron_aux][['IDDDName']], length(out))
                            return(out)
                        })

                        return(Var)
                    })

                    metaVar <- unlist(metaVar)
                    #outputNew <- setdiff(outputNewName, metaVar)
                    if (length(metaVar) > 0) {

                        outputMetaVar <- data.table(UnitName = metaVar, IDDDName = names(metaVar))

                    } else {

                        outputMetaVar <- data.table()

                    }
                } else {

                    outputMetaVar <- data.table()
                    #outputNew <- outputNewName
                }

                #outputNew <- data.table(UnitName = outputNew, IDDDName = outputNew)
                output <- output[which(output[['UnitName']] %in% UnitNamesLocal),
                                 c('UnitName','IDDDName'), with = F]
                output <- rbindlist(list(output, outputMetaVar))
                out <- output[['IDDDName']]
                names(out) <- output[['UnitName']]
                out <- out[UnitNamesLocal]
                return(out)
            }
            VNCNames <- unique(VNC[['UnitName']])
            UnitNamesLocal <- intersect(UnitNames, VNCNames)
            UnitNamesLocalNewName <- setdiff(UnitNames, UnitNamesLocal)
            UnitNamesLocal <- c(UnitNamesLocal, UnitNamesLocalNewName)
            namesLocal <- UnitToIDDDNames.local(UnitNamesLocal)
            outDT <- data.table(Unit = names(namesLocal), IDDD = namesLocal)
            outDT <- outDT[Unit %in% UnitNames]
            return(outDT)
        })

        outDT <- rbindlist(output.list)
        setkeyv(outDT, names(outDT))
        outDT <- outDT[!duplicated(outDT, by = key(outDT))]
        outDT[, Suffixes := gsub('([A-Za-z]+_)((\\[.*)|(.*))','\\2', Unit)]
        outDT.list <- split(outDT, outDT[['Suffixes']])
        outDT.list <- lapply(outDT.list, function(DT){

            DT[, IDDD := gsub('..', unique(Suffixes), IDDD, fixed = TRUE)]
            DT[, Suffixes := NULL]
            return(DT)
        })
        outDT <- rbindlist(outDT.list)
        outVector <- outDT[['IDDD']]
        names(outVector) <- outDT[['Unit']]
        outVector <- outVector[UnitNames]
        return(outVector)
    }

)

#' @rdname UnitToIDDDNames
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
