

#-------------------------------------------------------------------------------------------------
#' get.tiger
#'
#' This function downloads TIGER shapefiles from the US Census. It is designed for version 2010 and later. In most cases it will not work for earlier versions. This only needs to be done once.
#' @param fn The file name to be downloaded. This takes the format of tl_<version>_<extent>_<layer><year>. Either fn or the version, year and extent must be provided.
#' @param layer The layer of the file to be downloaded. Examples include: state, county, tract, TABBLOCK (census block), or ZCTA5 (five digit sip code tabulation area). 
#' @param extent A parent geography id.  This can be a five-digit county fips, two-digit state fips, or "us" for the whole county. It should be provided as a character. Not all extents are available for all layers.
#' @param yy The two-digit year provided as a character.  This is is most likely "00" or "10" for the two decennial census years.  
#' @param src The source of the file. Only TIGER is available now. This will be extended to the smaller GENZ files in the future.
#' @param version The version that hsould be used. The default is 2010. This function has limited capabilities for versions before 2010.
#' @param loc.path The local path where the data should be downloaded to.
#' @author Ander Wilson
#' @export
#' @examples
#' # Download the 2010 state shape file for the entire US. 
#' # This will download an approximately 8.3Mb file and may take a few minutes.
#' # This only needs to be done once.
#' tiger.info <- get.tiger(fn = "tl_2010_us_state10")
#' 


get.tiger <- function(fn,extent,layer,yy,src="tiger",version=2010, loc.path){

  if(missing(loc.path)) loc.path <- getwd()
  if(missing(fn)){
    if(missing(extent) & missing(layer) & missing(yy)){
      message("Either fn or extent, layer, and yy must be provided.")
      return()
    }
    fn <- make.fn(extent=extent,layer=layer,yy=yy,src=src,version=version)
  }
  
  #check to make sure file is available
  ftf <- find.tiger.file(fn)
  if(is.null(ftf)) return()
  
  #download the file
  download.results <- download.tiger(fn = ftf$fn,loc.path = loc.path)
  
  return(download.results)
}




