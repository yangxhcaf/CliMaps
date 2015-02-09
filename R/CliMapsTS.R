
#-------------------------------------------------------------------------------------------------
#' CliMapsTS
#'
#' This function takes a link from the function CliMaps and an array of gridded data and creates a data.frame of time series data.
#' @param link A link from from the function CliMaps.
#' @param data An array of of data. The dimensions should be 1) longitude, 2) latitude, and 2) time. If data is provided than CliMaps both links the data and creates the time series. If data is not provided then CliMaps only creates the link. 
#' @param names A list of names for the dimensions of data. The names for time will be the colnames of the timeseries. The vectors in the list should be named lon, lat, and time.
#' @author Ander Wilson
#' @export


CliMapsTS <- function(
  link,
  data,
  names
){
  
  #talk link object
  if(!is.data.table(link) & is.list(link)) link <- data.table(link$link)
  
#   #check order of data
#   data <- aperm(data, perm = c(which(names(names)=="lon"), which(names(names)=="lat"), which(names(names)=="time")))
#   names <- names[c("lon","lat","time")]
#   
  #match series
  if(!missing(names)){
    series <- data.frame(row.names=names$time)    
  }else{
    series <- data.frame()   
  }

  for(id in as.character(unique(link$ID))){
    if(nrow(link[ID==id])==1){
      series[,id] <- drop(data[unlist(link[ID==id,list(lon_id)]),unlist(link[ID==id,list(lat_id)]),])  
    }else{
      temp <- NULL
      for(i in 1:length(unlist(link[ID==id,list(lon_id)]))){
        temp <- cbind(temp,drop(data[unlist(link[ID==id,list(lon_id)])[i],unlist(link[ID==id,list(lat_id)])[i],]))
      }
      series[,id] <- drop(temp%*%unlist(link[ID==id,list(weight)]))
    }
  }

  
  return(series)
}