#' @title Convert IDDD variable names into their corresponding unit\emph{j} names
#'
#' @description \code{IDDDToUnitNames} returns a character vector with the unit
#' variable name for each IDDD variable name.
#'
#' @param object Object with the IDDD variable identifiers.
#'
#' @return Character vector with all the variable names.
#'
#' @examples
#' IDDDToUnitNames(ExampleQ)
#'
#' @import data.table
#'
#' @export
setGeneric("IDDDToUnitNames", 
           function(IDDDNames, VNC, Unit = 'Unit1'){standardGeneric("IDDDToUnitNames")})

#' @rdname IDDDToUnitNames
#'
#' @include DD-class.R
#'
#' @import data.table
#' 
#' @export
setMethod(
    f = "IDDDToUnitNames",
    signature = c("character", "VarNameCorresp"),
    function(IDDDNames, VNC, Unit = 'Unit1'){
        
        output <- matrix(character(2), ncol = 2, nrow = 1, 
                         dimnames = list(NULL, c('IDDDName', 'Unit')))
        for (VNCslot in names(VNC@VarNameCorresp)) {
            
            XLS <- as.data.table(copy(VNC@VarNameCorresp[[VNCslot]]))
            ColsUnit <- names(XLS)[grep('Unit', names(XLS))]
            ColsNotUnit <- setdiff(names(XLS), ColsUnit)
            XLS[, IDDDName := '']
            for (col in ColsNotUnit) {
            
                XLS[, IDDDName := paste(IDDDName, get(col), sep ='_')]
                XLS[, IDDDName := gsub('^_+', '', IDDDName)]
                XLS[, IDDDName := gsub('_+$', '', IDDDName)]
            }
            output <- rbind(output, as.matrix(XLS[, c('IDDDName', Unit), with = F]))

        }
        output <- output[!duplicated(output[, 'IDDDName']), ]
        output <- output[output[, 'IDDDName'] != '',]
        outVector <- output[, 'Unit']
        names(outVector) <- output[, 'IDDDName']
        outVector <- outVector[IDDDNames]
        names(outVector) <- NULL
        return(outVector)
    }
)


