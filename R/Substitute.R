#' @title Substitute values of variables of some statistical units from an
#' object into another object
#'
#' @description This method implements the substitution of values of variables
#' specified as an input parameter of a set of statistical units also
#' specified as an input parameter of an input object taken the corresponding v
#' alues from another input object.
#'
#' @param In Object which values will be sustituted into.
#'
#' @param From Object which values will be taken from.
#'
#' @param VarNames Character vector with the names of the variables whose values
#' will be substituted.
#'
#' @param Units \linkS4class{data.table} with the values of the statistical unit
#' qualifiers whose values are to be substituted.
#'
#' @return This method returns an object with the same class as the input object
#'  \code{In} with the values of the input variable substituted. If the input
#'  parameter \code{Units} is missing, the substitution will be carried out
#' in every statistical unit of the object \code{From}.
#'
#' @examples
#' # Creamos dos conjuntos de datos
#' library(data.table)
#' Data1 <- data.table(NOrden = c('000002896SS', '000003986SS'),
#'                     EsRemuner = c('', ''),
#'                     IDDD = c('CifraNeg', 'CifraNeg'),
#'                     Value = c(0, 187364))
#'
#' Data2 <- data.table(NOrden = c('000002896SS', '000003986SS'),
#'                     EsRemuner = c('', ''),
#'                     IDDD = c('CifraNeg', 'CifraNeg'),
#'                     Value = c(10000, 188364))
#'                     
#' # Creamos el objeto VarNameCorresp
#' VNCList <- list(Microdata = 
#'                  data.table(IDQual = c('NOrden', '', '', '', '', '', '', ''),
#'                             NonIDQual = c('', 'EsRemuner', '', '', '', '', '', ''), 
#'                            IDDD = c('', '', 'Mes', 'Anno', 'CCAA',
#'                                     'CNAE2009', 'CifraNeg', 'Empleo'), 
#'                            NOrden = c('', '', '', '', '', '', '', ''), 
#'                            EsRemuner = c('', '', '', '', '', '', '', '1'), 
#'                            Unit1 = c('', '', 'Mes', 'anio', 'ccaa', 'cnae09', 'CN', 'EmpRem')))
#' VNC <- new(Class = 'VarNameCorresp', VarNameCorresp = VNCList)
#' 
#' # Creamos un slot DD:
#' MicroDD <- data.table(Variable = c('NOrden',
#'                                    'EsRemuner',
#'                                    'Mes',
#'                                    'Anno',
#'                                    'CCAA',
#'                                    'CNAE2009',
#'                                    'CifraNeg',
#'                                    'Empleo'),
#'                       Sort = c('IDQual',
#'                                'NonIDQual',
#'                                'IDDD',
#'                                'IDDD',
#'                                'IDDD',
#'                                'IDDD',
#'                                'IDDD',
#'                                'IDDD'),
#'                       Class = c('character',
#'                                 'integer',
#'                                 'character',
#'                                 'character',
#'                                 'character',
#'                                 'character',
#'                                 'numeric',
#'                                 'integer'),
#'                       Qual1 = c('', '', 'NOrden', 'NOrden', 'NOrden', 
#'                                 'NOrden', 'NOrden', 'NOrden'),
#'                       Qual2 = c('', '', '', '', '', '', '', 'EsRemuner'))
#' DD <- new(Class = 'DD', MicroData = MicroDD, VarNameCorresp = VNC)
#'
#' # Finalmente creamos el objeto
#' Q1 <- new(Class = 'StQ', Data = Data1, DD = DD)
#' Q2 <- new(Class = 'StQ', Data = Data2, DD = DD)
#' #Substitute(In = Q1, From = Q2, 'CifraNeg', data.table(NOrden = '000002896SS'))
#'
#' @export
setGeneric("Substitute", function(In,
                                  From,
                                  VarNames,
                                  Units) {standardGeneric("Substitute")})

#' @rdname Substitute
#'
#' @include StQ-class.R dcast_StQ.R getDD.R getUnits.R setVar.R getDD.R
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "Substitute",
  signature = c("StQ", "StQ", "vector", "data.table"),
  function(In,
           From,
           VarNames,
           Units){

    UnitsFrom <- getUnits(From)
    setkeyv(UnitsFrom, names(UnitsFrom))

    if (missing(Units)) {

      Units <- getUnits(In)

    }
    setkeyv(Units, names(Units))
    CommonUnits <- UnitsFrom[Units]
    if (!identical(Units, CommonUnits)) {

        stop('[StQ::Susbtitute] Input parameters Units and From do not have the same units.')

    }
    From.dt <- dcast_StQ(From, VarNames)
    setkeyv(From.dt, names(Units))

    In.dt <- dcast_StQ(In, VarNames)
    setkeyv(In.dt, names(Units))

    for (Var in VarNames){
        In.dt[Units, Var := From.dt[Units][[Var]], with = F]
    }
    
    ### FALTA POR DEPURAR ESTA PARTE. HAY UN PROBLEMA DE SCOPING
    
    output <- copy(In)
    for(Var in VarNames){
      auxnewDD <- getDD(In)[Variable == Var]    
      Val <- From.dt[[Var]]
      output <- setVar(output, newDD = auxnewDD, Value = Val)
    }

    return(output)

  }
)

#' @rdname Substitute
#'
#' @include StQ-class.R getUnits.R
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "Substitute",
  signature = c("StQ", "StQ", "list"),
  function(In,
           From,
           VarNames,
           Units){

    UnitsFrom <- getUnits(From)
    setkeyv(UnitsFrom, names(UnitsFrom))

    if (length(VarNames) != dim(Units)[[1]]) {

        stop('[StQ::Substitute] VarNames and Units must have the same number of units.')
    }

    output <- In
    for (i in seq(along = VarNames)){

      output <- Substitute(In = output, From = From, VarNames = VarNames[[i]], Units = Units[i])

    }

    return(output)

  }
)
