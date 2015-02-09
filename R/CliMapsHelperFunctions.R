

unpack <- function(l){
  temp <- data.table(lon=unlist(lapply(l@Polygons, function(l2) l2@coords[,1])),
                     lat=unlist(lapply(l@Polygons, function(l2) l2@coords[,2])),
                     hole=unlist(lapply(l@Polygons, function(l2) rep(l2@hole,nrow(l2@coords)))),
                     piece=1,
                     ID=l@ID)
  dims <- cumsum(c(0,unlist(lapply(l@Polygons, function(l2) nrow(l2@coords)))))
  for(i in 1:length(l@plotOrder)) temp[(dims[i]+1):dims[i+1],piece:=l@plotOrder[i]]
  temp[,order:=1:nrow(temp)]
  temp[,ID2:=paste(temp$ID,temp$piece,sep=".")]
  return(temp)
} 


area.weight <- function(dt.temp,lat.step, lon.step,lat.grid,lon.grid, prj){
  df.temp <- NULL
  for(i in min(unlist(unique(dt.temp[,list(lat_id)]))):max(unlist(unique(dt.temp[,list(lat_id)])))){
    for(j in min(unlist(unique(dt.temp[,list(lon_id)]))):max(unlist(unique(dt.temp[,list(lon_id)])))){
      inter <- gIntersection(SpatialPolygons(list(Polygons(list(Polygon(cbind(ll.trans(dt.temp[,list(lon)]),dt.temp[,list(lat)]))),1))),
                             SpatialPolygons(list(Polygons(list(Polygon(cbind(ll.trans(lon.grid[j])+c(lon.step/2,-rep(lon.step,2)/2,rep(lon.step,2)/2),lat.grid[i]+c(rep(lat.step,2)/2,-rep(lat.step,2)/2,lat.step/2)))),1)))
                             )
      if(!is.null(inter)) df.temp <- rbind(df.temp,data.frame(ID=unique(dt.temp$ID),lon_id=j,lat_id=i, weight=gArea(inter)))
    }}
  return(df.temp)
}




check.sp <- function(sp, IDvar){
  
  if(class(sp)[[1]]=="SpatialPolygonsDataFrame"){
    #make data.table or location id info
    dat.ids <- data.table(attr(sp,"data"))
    dat.ids[,ID:=row.names(attr(sp,"data"))]
    setkeyv(dat.ids,"ID")    
    
    #add coordinates
    dat.coords <- data.table(ldply(sp@polygons,unpack))
    setkeyv(dat.coords,c("ID","order","ID2"))
    
    dat.map <- dat.ids[dat.coords]
    setkeyv(dat.map,c("ID","order","ID2"))
    rm(list=c("dat.coords"))
    gc()
    
    return(list(sp,dat.map,dat.ids))  
  }else if(class(sp)[[1]]=="SpatialPolygons"){
    #add coordinates
    dat.coords <- data.table(ldply(sp@polygons,unpack))
    setkeyv(dat.coords,c("ID","order","ID2"))
    
    dat.ids <- unique(dat.coords[,list(ID)])
    dat.ids[,name:=names(sp)]
    setkeyv(dat.ids,"ID")    
    
    dat.map <- dat.ids[dat.coords]
    setkeyv(dat.map,c("ID","order","ID2"))
    rm(list=c("dat.coords"))
    gc()
    
    return(list(sp,dat.map,dat.ids))  
  }else if(is.data.frame(sp)){
    sp <- data.table(sp)
    
    #check ID variable
    if(is.null(IDvar)& !any(toupper(colnames(sp))=="ID") & !any(colnames(sp)==IDvar)){
      #if no IDvar, no variable called ID, or no variable called IDvar then error.
      message("ERROR: No valid shape ID variable (IDvar) provided. Either add a variable named ID to the data.frame sp or provide a valid IDvar.")
      return()
    }else if(!is.null(IDvar) & any(colnames(sp)==IDvar)){
      #IDvar is specified use that as ID
      setnames(sp,IDvar,"ID")
    }else if(!any(colnames(sp)=="ID") & any(toupper(colnames(sp))=="ID")){
      setnames(sp,colnames(sp)[which(toupper(colnames(sp))=="ID")],"ID")
    }
    
    #check order
    if(!any(toupper(colnames(sp))=="ORDER")){
      message("ERROR: No order variable provided.")
      return()
    }else if(any(toupper(colnames(sp))=="ORDER") & any(colnames(sp)=="order")){
      setnames(sp,colnames(sp)[which(toupper(colnames(sp))=="ORDER")],"order")
    }
    
    
    #check lon & lat variables
    if(!any(colnames(sp)=="lat")){
      if(any(toupper(colnames(sp))=="LAT")){
        setnames(sp,colnames(sp)[which(toupper(colnames(sp))=="LAT")],"lat")
      }else if(any(toupper(colnames(sp))=="LATTITUDE")){
        setnames(sp,colnames(sp)[which(toupper(colnames(sp))=="LATTITUDE")],"lat")
      }else{
        message("ERROR: Cannot find column lat in sp.")
        return()
      }
    }
    
    dat.coords <- sp
    setkeyv(dat.map,c("ID","order"))
    
    dat.ids <- unique(dat.coords[,list(ID)])
    setkeyv(dat.ids,"ID")    
    
    return(list(sp,dat.map,dat.ids))  
  }

}




parse.file.name <- function(fn){
  
  src <- substring(fn,1,gregexpr("_",fn)[[1]][1]-1)
  version <- substring(fn,gregexpr("_",fn)[[1]][1]+1,gregexpr("_",fn)[[1]][2]-1)
  extent <- substring(fn,gregexpr("_",fn)[[1]][2]+1,gregexpr("_",fn)[[1]][3]-1)
  layer <- substring(fn,gregexpr("_",fn)[[1]][3]+1,nchar(fn)-2)
  yy <- substring(fn,nchar(fn)-1,nchar(fn))
  year <- as.numeric(yy)+2000
  
  if(src=="tl"){
    src.full <- "TIGER"
  }else if(src=="gz"){
    src.full <- "GENZ"
  }
  
  return(list(src=src, src.full=src.full, version=version, extent=extent, layer=layer, yy=yy, year=year, fn=fn))
}



download.tiger <- function(fn,loc.path){
  
  #parse the file name 
  fn.details <- parse.file.name(fn)
  
  #specify location of file
  url.path <- paste0("ftp://ftp2.census.gov/geo/tiger/",fn.details$src.full,fn.details$version,"/",toupper(fn.details$layer),"/",fn.details$year,"/")
  
  #create data directory if not already present
  dir.create(paste(loc.path,fn,sep="/"), showWarnings = FALSE)
  
  #check contents to see if this fn is already downloaded.
  list.files(paste(loc.path,fn,sep="/"))
  #### here if only zip then skip download and unzip
  #### here if full data then skip download and unzip
  
  #download the file
  download.file(paste0(url.path,fn,".zip"), paste0(loc.path,"/",fn,"/",fn,".zip"))
  
  #unzip file
  untar(paste0(loc.path,"/",fn,"/",fn,".zip"), compressed = 'gzip', exdir = paste0(loc.path,"/",fn))
  
  #remove zip file
  file.remove(paste0(loc.path,"/",fn,"/",fn,".zip"))
  
  
  return(list(fn=fn,path=paste0(loc.path,"/",fn)))
}




make.fn <- function(extent,layer,yy,src="tl",version=2010){
  if(toupper(src)=="TIGER" || toupper(src)=="TL"){
    src <- "tl"
  }else if(toupper(src)=="GENZ" || toupper(src)=="GZ"){
    src <- "gz"
  }
  return(paste0(src,"_",version,"_",extent,"_",layer,yy))
}





find.tiger.file <- function(fn){
  
  fn.details <- parse.file.name(fn)
  
  #specify location of file
  url.path <- paste0("ftp://ftp2.census.gov/geo/tiger/",fn.details$src.full,fn.details$version,"/")
  #make sure version is available
  if(!url.exists(paste0(url.path))){
    message("File version not available. Check src and version.")
    return()
  }
  
  
  #make sure layer is available
  url.pathold <- url.path
  url.path <- paste0(url.path,toupper(fn.details$layer),"/")
  if(!url.exists(paste0(url.path))){
    message("File layer not available. Check layer.")
    message(paste0("For available option see: ",url.pathold))
    return()
  }
  
  
  #make sure year is available
  url.pathold <- url.path
  url.path <- paste0(url.path,"20",fn.details$yy,"/")
  if(!url.exists(paste0(url.path))){
    message("File year not available. Check yy.")
    message(paste0("For available option see: ",url.pathold))
    return()
  }
  
  #make sure file is available
  if(!(gregexpr(paste0(fn,".zip"),getURL(url.path))[[1]]>0)){
    message("File not available. Check extent.")
    message(paste0("For available option see: ",url.path))
    return()
  }
  
  return(list(path=url.path,fn=fn))
}


ll.trans <- function(x) return( (x+180)%%360-180 )
