# Download DEM from Kartverket's API
download_dem_kartverket <- function(stationid=18703, centre, name="dom",
                                    dx=100, resx=NA, f.overwrite=F){
  # Libraries
  require(ows4R) # WCSClient$new() getCapabilities()
  require(terra) # rast() setMinMax()

  print(sprintf("Process: %i - %1.1f/%1.1f - %s - %i/%i",stationid,centre[1],centre[2],name,dx,resx))
  # Parameters
  # dx   <- 1000
  #if(is.na(resx)){if(dx<2500){resx <- 1}else{resx <- 10}} # resX must be dx/100
  if(is.na(resx)){resx <- dx/100}
  # Verify if file exists
  path <- "~/Desktop/Projects/2023/Projects/met/station_metadata/station_location_files/dem"
  fname_out <- sprintf("%s/%i_%s_25833_d%05.0fm_%im.tif",path,stationid,name,dx,resx)
  if(file.exists(fname_out) && !f.overwrite){
    dem <- rast(fname_out)
    setMinMax(dem)
    print(sprintf("Load existing file: %s",fname_out))
    return(dem) }
  # Define bounding box
  box <- c(c(centre[1],centre[2])-dx,c(centre[1],centre[2])+dx) %>% round
  # Make url call
  url <- sprintf("https://wcs.geonorge.no/skwms1/wcs.hoyde-%s-nhm-25833",name)
  WCS <- WCSClient$new(url,serviceVersion = "1.0.0", logger = "INFO")
  caps <- WCS$getCapabilities()
  chla <- caps$findCoverageSummaryById(sprintf("nhm_%s_topo_25833",name), exact = T)
  # Download the data
  dem <- chla$getCoverage(crs = "EPSG:25833",RESX = resx, RESY = resx,
                          bbox=OWSUtils$toBBOX(box[1],box[3],box[2],box[4]),
                          filename = fname_out)
  # Assign Not-A-Number values and compute MinMax
  dem[dem==0] <- NA
  setMinMax(dem)
  return(dem)
}
