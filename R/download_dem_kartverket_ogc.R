#' Download DEM from Kartverket's NEWEST OGC API
#'
#' @param stationid A station ID used for the DEM file name
#' @param centre A coordinate array (i.e. `c(x, y)`) of the station in UTM 33 (i.e. `epsg:25833`)
#' @param name A name of the DEM to download, either `"dtm"` a terrain model or the default `"dom"` a surface model
#' @param dx A distance in metre or radius defining the extent of the bounding box from the centre point, default `100` metres
#' @param resx A horizontal resolution in metre, default is `dx/100` if greater than `1` metre
#' @param f.OGC A boolean default is `TRUE`
#'
#' @return A DEM
#' @export
#'
#'
#' @examples
#' download_dem_kartverket_ogc()
#'

download_dem_kartverket_ogc <- function(stationid=18703, centre, name="dom", dx=100, resx=1, f.OGC=T){

  # Define box limits
  box <- c(c(centre[1],centre[2])-dx,c(centre[1],centre[2])+dx) %>% round
  if(name=="dom"){name="dsm"}
  # Download URL
  url <- sprintf("https://ogcapitest.kartverket.no/geoe3/dem/collections/%s/coverage",name)
  # ?bbox=9.588317,61.0190519,9.716720,61.046985&f=GTiff
  # Assign Not-A-Number values
  dem[dem==0] <- NA
  return(dem)
}
