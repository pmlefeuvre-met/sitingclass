# Download DEM from Kartverket's NEWEST OGC API
download_dem_kartverket_ogc <- function(stationid=18703, centre, name="dom", dx=100, resx=1, f.OGC=T){
  require(ows4R)
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
