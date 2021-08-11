#' @title Method to transform an \link{StQ} object into a \linkS4class{StQList} object
#'
#' @description \code{StQToStQList} transform an \link{StQ} object (with a variable "Period"
#' taking into account the interval period times related to data) into the corresponding
#'  \linkS4class{StQList} object.
#'
#' The variable \code{Period} with the period related to each data will be removed from the DD
#' components in the new \linkS4class{StQList} object.
#'
#' @param object Object of class \link{StQ} to be transformed.
#'
#' @return object of class \linkS4class{StQList}.
#'
#' @include StQList.R StQ.R getData.R getDD.R getVNC.R BuildVNC.R BuildDD.R
#' 
#' @import data.table
#' 
#' @export
setGeneric("StQToStQList", function(object){standardGeneric("StQToStQList")})

#' @rdname StQToStQList
#'
#' @export
setMethod(
    f = "StQToStQList",
    signature = c("StQ"),
    function(object){
        
        IDQual <- Variable <- NULL
        
        Data <- getData(object)
        if (!('Period' %chin% names(Data))) stop('[StQ::StQToStQList] Component Data must have a column named Period.')
        
        DataList <- split(seq_len(nrow(Data)), Data$Period)
        DataList <- lapply(DataList, function(rows){Data[rows]})
        Periods <- names(DataList)
        Periods.dt <- data.table(Period = Periods, Order = RepoTime::orderRepoTime(Periods))
        Periods.dt <- setorderv(Periods.dt, 'Order')
        Periods <- Periods.dt$Period
        Data.output <- lapply(Periods, function(Period){
            
            out <- DataList[[Period]]
            out <- out[, Period := NULL]
            return(out)
        })


        DD <- getDD(object)
        VNC <- getVNC(DD)
        VNCNames <- names(VNC)
        VNC <- lapply(VNCNames, function(VNCName){
            
            out <- copy(VNC[[VNCName]])[IDQual != 'Period']
            out <- out[, setdiff(names(out), 'Period'), with = FALSE]
            return(out)
        })
        names(VNC) <- VNCNames
        VNC <- BuildVNC(VNC)
        
        DDNames <- setdiff(names(DD), 'VNC')
        DD <- lapply(DDNames, function(DDName){
            
            out <- copy(DD[[DDName]])[Variable != 'Period']
            DTNames <- names(out)
            NQuals <- DTNames[grep('Qual', DTNames)]
            out <- out[, setdiff(DTNames, NQuals[length(NQuals)]), with = FALSE]
            return(out)
        })
        names(DD) <- DDNames
        
        DD <- BuildDD(Data = list(VNC = VNC, ID = DD$ID, MicroData = DD$MicroData, ParaData = DD$ParaData, Aggregates = DD$Aggregates,
                                  AggWeights = DD$AggWeights, Other = DD$Other))
        
        StQ.List <- lapply(Data.output, StQ, DD)
        names(StQ.List) <- Periods
        
        output <- StQList(StQ.List, RepoTime::newRepoTime(Periods))
        
        return(output)
    }
)
