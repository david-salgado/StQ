#' @title Write a key-value pair txt file.
#' 
#' @description \code{RepoFiletoTXT} writes the key-value objects found in the
#' input parameter StQList in fixed-width column txt files. 
#' 
#' This method reads the files found with the input parameter StQList and writes
#' their content as a fixed-width column txt file with key-value pair structure
#' with the same name as files read, but extension .txt.
#' 
#' @param StQList Object of class \linkS4class{StQList}.
#' 
#' @param RutaEscritura Character vector of length 1 with the path where files
#' will be written. It must not be another txt file of the same periods which
#' are being considered.
#'  
#' @examples
#' \dontrun{
#'  StQList <- RepoFiletoStQ('Z:/', 'XXXXXX', 'C:/', 'E30183', 'MM112014', 'MM122014', 'FF')
#'  RepoFiletoTXT(StQList, 'C:/')
#' }
#'       
#' @export
    RepoFiletoTXT <- function(StQList, RutaEscritura){
        
        Data <- getData(StQList)
        namesData <- names(Data)
        FileType <- substr(namesData[1], 1, 2)
        
        for (i in seq(along = Data)){
            
            InitName <- paste0(Encuesta, '.', FileType, '_V1.')
            if (FileType == 'FF'){
                
                TextName <- paste0(RutaEscritura, InitName, substr(namesData[i], 4, 11), '.D_', RepoTopn(RutaEscritura, paste0(InitName,substr(namesData[i], 4, 11))), '.txt')
            
            }else{
                
                TextName <- paste0(RutaEscritura, InitName, substr(namesData[i], 4, 11), '.P_', RepoTopn(RutaEscritura, paste0(InitName, substr(namesData[i], 4, 11))), '.txt')
            }
            
            write.table(Data[[i]], TextName, quote = FALSE, sep=";") 
        }
}

