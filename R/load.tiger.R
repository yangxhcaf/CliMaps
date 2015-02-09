

#-------------------------------------------------------------------------------------------------
#' load.tiger
#'
#' This function takes a link from the function CliMaps and an array of gridded data and creates a data.frame of time series data.
#' @param fn A file name of the form tl_<version>_<extent>_<layer><year>.
#' @param path A local path where the TIGER or GENZ shape data are stored.
#' @author Ander Wilson
#' @export
#' @examples
#' # Download the 2010 state shape file for the entire US. 
#' # This will download an approximately 8.3Mb file and may take a few minutes.
#' # This only needs to be done once.
#' tiger.info <- get.tiger(fn = "tl_2010_us_state10")
#' 
#' # load the shape file
#' sp <- load.tiger(tiger.info$path,tiger.info$fn)

load.tiger <- function(path,fn){
  if(!file.exists(path)){
    message("Loading tiger data failed. Invalid path.")
    return()
  }  
  
  if(!file.exists(paste0(path,"/",fn,".shp"))){
    message("Loading tiger data failed. No .sph file in path.")
    return()
  }else if(!file.exists(paste0(path,"/",fn,".dbf"))){
    message("Loading tiger data failed. No .dbf file in path.")
    return()
  }else if(!file.exists(paste0(path,"/",fn,".shx"))){
    message("Loading tiger data failed. No .shx file in path.")
    return()
  }else if(!file.exists(paste0(path,"/",fn,".prj"))){
    message("No .prj file in path.")
    return()
  }
  
  return(readOGR(dsn=path, layer=fn))
}
