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
#' @return Returns a \linkS4class{data.table} with all the corresponding IDDD variable names. For
#' objects the classes \linkS4class{DD} and \linkS4class{StQ} it returns the IDDD the slot VarNameCorresp of the
#' corresponding DD object.
#'
#' @details IDDD and qualifiers compose together the so-called IDDDname of the variable by pasting
#' the IDDD identifier and each consecutive qualifier with an underscore _.
#'
#' @examples
#' # An example for VNC and DD objects:
#' library(data.table)
#' ### We build the VNC object
<<<<<<< HEAD
#' VarList <- list(ID = data.table(IDQual = c('NumIdEst', rep('', 4)),
=======
#' VarList <- list(ID = new(Class = 'VNCdt',
#'                          .Data = data.table(IDQual = c('NumIdEst', rep('', 4)),
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'                                             NonIDQual = c(rep('',5)),
#'                                             IDDD = c('', 'Name', 'Surname', 'PostalAddr',
#'                                                      'PhoneNo'),
#'                                             NumIdEst = c('', rep('.', 4)),
#'                                             UnitName = c('numidest', 'nombre', 'apellidos',
#'                                                       'direccion', 'telefono'),
<<<<<<< HEAD
#'                                             InFiles = rep('FF', 5)),
#'                 MicroData = data.table(
=======
#'                                             InFiles = rep('FF', 5))),
#'                 MicroData = new(Class = 'VNCdt',
#'                                 .Data = data.table(
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'                                         IDQual = c('NumIdEst', rep('', 2)),
#'                                         NonIDQual = c('', 'Market', ''),
#'                                         IDDD = c(rep('', 2), 'NewOrders'),
#'                                         NumIdEst = c(rep('', 2), '.'),
#'                                         Market = c(rep('', 2), '2.'),
#'                                         UnitName = c('numidest', '', 'cp09'),
<<<<<<< HEAD
#'                                         InFiles = rep('FF', 3)),
#'                 ParaData = data.table(
=======
#'                                         InFiles = rep('FF', 3))),
#'                 ParaData = new(Class = 'VNCdt',
#'                                 .Data = data.table(
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'                                         IDQual = c('NumIdEst', rep('', 2)),
#'                                         NonIDQual = c('', 'Action', ''),
#'                                         IDDD = c(rep('', 2), 'Date'),
#'                                         NumIdEst = c(rep('', 2), '.'),
#'                                         Action = c(rep('', 2), 'Imputation'),
#'                                         UnitName = c('numidest', '', 'FechaImput'),
<<<<<<< HEAD
#'                                         InFiles = rep('FP', 3)),
#'                 Aggregates = data.table(
=======
#'                                         InFiles = rep('FP', 3))),
#'                 Aggregates = new(Class = 'VNCdt',
#'                                  .Data = data.table(
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'                                          IDQual = c('Province', 'NACE09', '', ''),
#'                                          NonIDQual = c(rep('', 2), 'Market', ''),
#'                                          IDDD = c('', '', '', 'Turnover'),
#'                                          Province = c('', '', '', '.'),
#'                                          NACE09 = c('', '', '', '.'),
#'                                          Market = c('', '', '', '3.'),
#'                                          UnitName = c('provincia', 'actividad', '', 'cn01'),
<<<<<<< HEAD
#'                                          InFiles = rep('FA', 4)))
#'
#' VNC <- BuildVNC(VarList)
#'
#' ### We build the specification data.tables
#' IDdt <- new(data.table(
=======
#'                                          InFiles = rep('FA', 4))))
#'
#' VNC <- new(Class = 'VarNameCorresp', .Data = VarList)
#'
#' ### We build the specification data.tables
#' IDdt <- new(Class = "DDdt",
#'             .Data = data.table(
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'                      Variable = c('NumIdEst', 'Name', 'Surname', 'PostalAddr', 'PhoneNo'),
#'                      Sort = c('IDQual', rep('IDDD', 4)),
#'                      Class = rep('character', 5),
#'                      Length = c('11', '25', '25', '50', '9'),
#'                      Qual1 = c('', rep('NumIdEst', 4)),
#'                      ValueRegExp = c('[0-9]{9}PP', '.+', '.+', '.+', '(6|9)[0-9]{8}')))
<<<<<<< HEAD
#' Microdt <- data.table(
=======
#' Microdt <- new(Class = "DDdt",
#'             .Data = data.table(
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'                      Variable = c('NumIdEst', 'Market', 'NewOrders'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = c(rep('character', 2), 'numeric'),
#'                      Length = c('11', '2', '7'),
#'                      Qual1 = c(rep('', 2), 'NumIdEst'),
#'                      Qual2 = c(rep('', 2), 'Market'),
#'                      ValueRegExp = c('[0-9]{9}PP', '(0|1| )', '([0-9]{1, 10}| )')))
<<<<<<< HEAD
#' Paradt <- data.table(
=======
#' Paradt <- new(Class = "DDdt",
#'             .Data = data.table(
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'                      Variable = c('NumIdEst', 'Action', 'Date'),
#'                      Sort = c('IDQual', 'NonIDQual', 'IDDD'),
#'                      Class = rep('character', 3),
#'                      Length = c('11', '4', '10'),
#'                      Qual1 = c(rep('', 2), 'NumIdEst'),
#'                      Qual2 = c(rep('', 2), 'Action'),
#'                      ValueRegExp = c('[0-9]{9}PP', 'Collection|Editing|Imputation',
#'                      '(([0-9]{2}-(0[1-9]|1(0-2))-[0-9]{4})| )')
#' ))
<<<<<<< HEAD
#' Aggdt <- data.table(
=======
#' Aggdt <- new(Class = "DDdt",
#'             .Data = data.table(
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'                      Variable = c('Province', 'NACE09', 'Turnover'),
#'                      Sort = c(rep('IDQual', 2), 'IDDD'),
#'                      Class = c(rep('character', 2), 'numeric'),
#'                      Length = c('25', '4', '12'),
#'                      Qual1 = c(rep('', 2), 'Province'),
#'                      Qual2 = c(rep('', 2), 'NACE09'),
<<<<<<< HEAD
#'                      ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )'))
#'
#' DD <- DD(VNC = VNC,
=======
#'                      ValueRegExp = c('[0-9]{4}', '([0-4][0-9])|(5[0-2])', '([0-9]{1, 15}| )')))
#'
#' DD <- new(Class = 'DD',
#'           VarNameCorresp = VNC,
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#'           ID = IDdt,
#'           MicroData = Microdt,
#'           ParaData = Paradt,
#'           Aggregates = Aggdt)
#'
#' StQ <- new(Class = 'StQ', Data = new(Class = 'Datadt'), DD = DD)
#'
#' UnitToIDDDNames(DD, UnitNames = c('cn01', 'cp09'))
#'
#' UnitToIDDDNames(StQ, UnitNames = c('cn01', 'provincia', 'cp09'))
#'
<<<<<<< HEAD
#' @include getIDQual.R VNC.R DD.R BuildVNC.R BuildDD.R StQ.R getDD.R
#' 
=======
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
#' @export
setGeneric("UnitToIDDDNames", function(UnitNames, Correspondence){standardGeneric("UnitToIDDDNames")})

#' @rdname UnitToIDDDNames
<<<<<<< HEAD
#' 
#' @include DD.R VarNamesToDD.R getVNC.R BuildDD.R BuildVNC.R VNC.R
=======
#'
#' @include VNCdt-class.R getIDQual.R VarNameCorresp-class.R DatadtToDT.R
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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
<<<<<<< HEAD
            XLS <- VNC[UnitName %in% UnitNames]
=======
            XLS <- slot(VNC, '.Data')
            names(XLS) <- names(VNC)
            setDT(XLS)
            XLS <- XLS[UnitName %in% UnitNames]

>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
            XLS[, IDDDName := IDDD]
            XLS.Quals <- XLS[IDDD == '']
            XLS.Quals[IDQual != '', IDDDName := IDQual]
            XLS.Quals[NonIDQual != '', IDDDName := NonIDQual]
            XLS.Quals <- XLS.Quals[, c('IDQual', 'NonIDQual', 'UnitName', 'IDDDName', 'InFiles'),
                                   with = F]
            IDQual <- XLS.Quals[IDQual != '']
            IDQual <- IDQual[IDQual != '']

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
<<<<<<< HEAD
                auxDT <- DD[[nameVNC]][Variable == unique(xls[['IDDDName']])]
=======
                auxDT <- DatadtToDT(slot(DD, nameVNC))[Variable == unique(xls[['IDDDName']])]
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
                ColsNotUnit <- t(as.matrix(auxDT[, names(auxDT)[grep('Qual', names(auxDT))], with = FALSE]))[,1]
                ColsNotUnit <- setdiff(ColsNotUnit, IDQual)
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
                        patron <- gsub('\\[n\\]', '[0-9]+', patron)
                        patron <- gsub('\\[varGestion\\]', '.*', patron)
                        patron <- gsub('\\[ccaa\\]', '[0-9]{2}', patron)
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
<<<<<<< HEAD
=======
#' @include DD-class.R VarNamesToDD.R getVNC.R
#'
#' @import data.table
#'
#' @export
#setMethod(
#    f = "UnitToIDDDNames",
#    signature = c("character", "DD"),
#    function(UnitNames, Correspondence){


#        VNC <- getVNC(Correspondence)

#        output <- UnitToIDDDNames(UnitNames, VNC)

#        return(output)

#    }
#)

#' @rdname UnitToIDDDNames
#'
#' @include StQ-class.R
#'
>>>>>>> 5034523f22c62817420f2f5687369d62b4523cd8
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