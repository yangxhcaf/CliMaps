#-------------------------------------------------------------------------------------------------
#' CliMaps
#'
#' This function links each geographical unites to grid cells. The linking is area weighted to each grid.
#' @param sp The spatial data. The data can be of class spatialpolygon, spatialpolygondataframe, or a data.frame with with columns for longitude and latitude. Each spatial unit in sp will be linked to the gridded data.
#' @param lon.grid A numeric vector of numeric longitudes for the centroids of each gridcell
#' @param lat.grid A numeric vector of numeric lattitudes for the centroids of each gridcell
#' @param data Option array of of data. The dimensions should be 1) longitude, 2) latitude, and 2) time. If data is provided than CliMaps both links the data and creates the time series. If data is not provided then CliMaps only creates the link. 
#' @param names An option list of names for the dimensions of data. The names for time will be the colnames of the timeseries. The vectors in the list should be named lon, lat, and time.
#' @param DT Ligical indicating if a data.table should be returned. If FALSE (default) a data.frame will be returned.
#' @param IDvar A optional variable to be used as an ID if the spatial data is provided as a data.frame. This is not used if an opject of class sp is provided.
#' @param grid.tolerance This is a tolerance level used to make sure the grid is evenly spaced. If the grid is not evenly spaced it can still be treated as evenly spaced by setting a high tolderance. 
#' @author Ander Wilson
#' @export


CliMaps <- function(sp,lon.grid,lat.grid,data,names,DT=FALSE,IDvar,grid.tolerance=0.0001){
  
  #check the format of the sp file. 
  if(missing(IDvar)) IDvar <- NULL
  spc <- check.sp(sp, IDvar=IDvar)
  if(is.null(sp)) return()
  sp <- spc[[1]]
  dat.map <- spc[[2]]
  dat.ids <- spc[[3]]
  

  #find grid spacing
  if(sd(lat.grid[-1]-lat.grid[-length(lat.grid)])/mean(lat.grid[-1]-lat.grid[-length(lat.grid)])>grid.tolerance){
    message("Invalid lat.grid provided. It is either out of order or not evenly spaced.")
    return()
  }
  if(sd(lon.grid[-1]%%360-lon.grid[-length(lon.grid)]%%360)/mean(lon.grid[-1]-lon.grid[-length(lon.grid)])>grid.tolerance){
    message("Invalid lon.grid provided. It is either out of order or not evenly spaced.")
    return()
  }
  lat.step <- abs(mean(lat.grid[-1]-lat.grid[-length(lat.grid)]))
  lon.step <- abs(mean(lon.grid[-1]%%360-lon.grid[-length(lon.grid)]%%360))

  
  
  
  
  #find any units that extend beyond grid
  minlondif <- function(lon) return(as.numeric(min(pmin(abs(ll.trans(lon.grid)-ll.trans(lon)),360-abs(ll.trans(lon.grid)-ll.trans(lon))))))
  minlatdif <- function(lat) return(as.numeric(min(abs(lat.grid-lat))))
  dat.map[,min.lon.dif:=unlist(lapply(lon, minlondif))]
  dat.map[,min.lat.dif:=unlist(lapply(lat, minlatdif))]
  nomatch <- dat.map[min.lon.dif>lon.step/2 | min.lat.dif>lat.step/2]
  nomatch <- unique(nomatch, by=c("ID"))
  nomatch <- nomatch[,lon:=NULL]
  nomatch <- nomatch[,lat:=NULL]
  nomatch <- nomatch[,order:=NULL]
  nomatch <- nomatch[,piece:=NULL]
  nomatch <- nomatch[,hole:=NULL]
  setkeyv(nomatch,c("ID"))
  
  #remove the ones that aren't matched
  dat.map <- dat.map[!ID%in%nomatch$ID]
  
  #determine cell each vertex is in
  lonidf <- function(lon) return(as.numeric(which.min(pmin(abs(ll.trans(lon.grid)-ll.trans(lon)),360-abs(ll.trans(lon.grid)-ll.trans(lon))))))
  dat.map[,lon_id:=unlist(lapply(lon, lonidf))]
  latidf <- function(lat) return(as.numeric(which.min(abs(lat.grid-lat))))
  dat.map[,lat_id:=unlist(lapply(lat, latidf))]
  
  
  
  which.min(abs(ll.trans(lon.grid)-ll.trans(179.9)))
  lon.grid[72]
  
  #determine how many cells each unit falls in
  grid.level <- unique(dat.map,by=c("ID","lon_id","lat_id"))
  grid.level[,totcells:=length(lon_id),by="ID"]
  
  #store counties in one cell
  links <- grid.level[totcells==1,list(ID,lon_id,lat_id)]
  links[,weight:=1]
  
  
  #match and store counties with multiple cells
  for(g_id in unlist(unique(grid.level[totcells>1,list(ID)])))
    for(p in unique(dat.map[ID==g_id]$piece))
      links <- rbind(links,area.weight(dat.map[ID==g_id & piece==p],lat.step, lon.step,lat.grid,lon.grid))
  
  #scale weights
  links[ ,weight:=weight/sum(weight),by="ID"]
  
  setkeyv(links,c("ID","lon_id","lat_id"))
  links <- links[dat.ids,nomatch=0]
  
  dat.map <- dat.map[ID%in%links$ID]
  
  if(missing(data)){
    if(DT){
      return(list(link=links,nomatch=nomatch,dat.map=dat.map))    
    }else{
      return(list(link=as.data.frame(links),nomatch=as.data.frame(nomatch),dat.map=as.data.frame(dat.map)))
    }    
  }else{
    if(!missing(names)){
      ts <- CliMapsTS(links,data,names)
    }else{
      ts <- CliMapsTS(links,data)      
    }
    if(DT){
      return(list(link=links,nomatch=nomatch,dat.map=dat.map, ts=data.table(ts)))    
    }else{
      return(list(link=as.data.frame(links),nomatch=as.data.frame(nomatch),dat.map=as.data.frame(dat.map), ts=ts))
    }    
  }
  
}
