#' @title Produce an object of class \linkS4class{StQList} from files with the
#' structure key-value pair.
#' 
#' @description \code{RepoFiletoStQList} returns an object of class
#' \linkS4class{StQList} from files with the structure key-value pair.
#' 
#' @param RutaRepo Character vector of length 1 with the letter of the unit to
#' be mapped. It must be written in the form: 'Z:/' (for example).
#' 
#' @param RutaEscritura Character vector of length 1 with the path where files
#' to be read are.
#' 
#' @param Encuesta Character vector of length 1 with the name of each survey
#' (for example, 'E30183').
#' 
#' @param MesInicial Character vector of length 1 with the first month to be
#' read, in the repository notation (for example, 'MM102014').
#' 
#' @param MesFinal Character vector of length 1 with the last month to be
#' read, in the repository notation (for example, 'MM102014').
#' 
#' @param TipoFichero Character vector of length 1 with the type of the file to
#' be read. It must be 'FF', 'FG' or 'FD'.
#' 
#' @return StQList Object of class \linkS4class{StQList}.
#' 
#' @examples
#' \dontrun{
#'  RepoFiletoStQList('Z:/', 'XXXXXX', 'C:/', 'E30183', 'MM102014', 'MM102014', 'FF')
#' }
#'
#' @include 
#' 
#' @import data.table 
#' 
#' @importFrom xlsx loadWorkbook
#'       
#' @export
RepoFileToStQList <- function (RutaEscritura, Encuesta, MesInicial, MesFinal, TipoFichero){
    
        if (TipoFichero != 'FF' & TipoFichero != 'FG' & TipoFichero != 'FD'){
            
            stop('Los ficheros a escribir sólo pueden ser de tipo FF, FG o FD')
        }
        
        IniRepoTime <- newRepoTime(MesInicial)
        FinRepoTime <- newRepoTime(MesFinal)
        Months <- Seq(IniRepoTime, FinRepoTime, Rot = FALSE)
        MonthsNamesM <- getRepo(Months)
        
        #####                 Creación de lista de objetos StQ             #####
        QList <- list()
        
        
        ExcelName <- paste0(RutaRepo, Encuesta, '.NombresVariables', '.xlsx')
        wb <- loadWorkbook(ExcelName)
        SheetNames <- names(getSheets(wb))
        SheetNames[SheetNames != 'ID']
        
        
        if (!'ParaData' %in% SheetNames){
        
            VNC <- RepoXLSToVNC(ExcelName, SheetNames)
        }else{
        
            SheetNames1 <- SheetNames[SheetNames != 'ParaData']
            SheetNames2 <- c('ParaData')
            VNC1 <- RepoXLSToVNC(ExcelName, SheetNames1)
            VNC2 <- RepoXLSToVNC(ExcelName, SheetNames2)
            VNC <- VNC1 + VNC2
        }
        
        
        DDName <- paste0(RutaRepo, Encuesta, '.DD_V', RepoTopn(RutaRepo, 'DD'))
        RepoDD <- ReadRepoFile(DDName)
        DD <- RepoDDToDD(RepoDD, VNC)
        
        for (Month.index in seq(along = MonthsNamesM)) {
            
            InitName <- paste0(Encuesta, '.', TipoFichero, '_V1.')
            if (TipoFichero == 'FF'){
                
                Name <- paste0(RutaEscritura, InitName, MonthsNamesM[[Month.index]], '.D_', RepoTopn(RutaEscritura, paste0(InitName, MonthsNamesM[[Month.index]])))
            }else{
                
                Name <- paste0(RutaEscritura, InitName, MonthsNamesM[[Month.index]], '.P_', RepoTopn(RutaEscritura, paste0(InitName, MonthsNamesM[[Month.index]])))
            }
            
            Repo <- ReadRepoFile(Name)
            QList[[Month.index]] <- new(Class = 'StQ', Data = Repo, DD = DD)
        }
        
        
        # Desconexión a la unidad de red
        UnMapDrive(substr(RutaRepo, 1, 2))
        if (MappingStatus(Encuesta, substr(RutaRepo, 1, 2))){
            
            stop('La unidad de red no se ha podido desconectar.')
        }
        
        
        names(QList) <- MonthsNamesM
        StQList <- BuildStQList(QList)
        
        names(StQList@Data) <- paste0('FF_', names(StQList@Data))
        
        return(StQList)
}

