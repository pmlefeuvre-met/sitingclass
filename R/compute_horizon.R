#' Horizon height at a station
#'
#' Compute horizon height in degrees from a station location in UTM 33
#'
#' @references \url{https://grass.osgeo.org/grass83/manuals/r.horizon.html}
#'
#' @param centre An array of two coordinates in UTM 33 (epsg:25833)
#' @param dem A SpatRaster of a digital elevation/surface model in UTM 33 (epsg:25833)
#' @param level A height above the ground of the sensor in metres, default level `0` is set to `2` metres
#' @param step An interval in degrees at which the horizon will be computed, default is every `10` deg.
#' @param f.plot.polygon A boolean setting boundary values to -20 deg to facilitate plotting as polygon
#'
#' @return horizon_height in degrees and azimuth angle at which the horizon is computed in degrees
#'
#' @examples
#' compute_horizon(stn.centre,dem,level=stn.level,step=.01,f.plot.polygon=T)
#' compute_horizon(stn.centre,dem,level=stn.level,step=.01,f.plot.polygon=T)
#'
#' @export
compute_horizon <- function(centre = NULL,
                            dem = NULL,
                            level = 2,
                            step = 10,
                            f.plot.polygon = F){
  # Libraries
  require(rgrass)
  require(terra) # cellFromXY()

  # Adjust ground level to match real sensor height
  #r.stn.level <- rast(ext(dem), resolution=res(dem), crs=crs(dem))
  #r.stn.level[100,100] <- 2
  #dem <- dem + buffer(r.stn.level,2)
  #j <-  adjacent(dem, cellFromXY(dem, centre), 4, include=T)
  j <-  cellFromXY(dem, centre)
  dem[c(j)] <- dem[c(j)] + ifelse(level==0,2,level)

  # Set GRASS path
  grasslib <- try(system('grass --config', intern=TRUE))[4]
  gisDbase <- 'data/grassdata/'

  # Initialise GRASS and projection
  initGRASS(gisBase=grasslib, home=tempdir(), SG=dem, gisDbase=gisDbase,
            mapset="PERMANENT",override=TRUE,remove_GISRC=TRUE)
  #execGRASS("g.proj", flags = "c", epsg = 25833)
  #execGRASS("g.region", flags = "p")

  # Load DEM
  write_RAST(dem, "elev", flags="o")
  #execGRASS("r.info", map="elev")

  # Compute horizon
  horizon <- execGRASS("r.horizon",flags=c('d','c','overwrite'),
                       parameters =list(elevation='elev', coordinates=round(centre[1:2],2),
                                        direction=90, distance=0.5,
                                        step=step, start=0, end=360),
                       intern = T) # DEBUG: echoCmd = T,

  # Construct Dataframe from GRASS output
  df <- horizon[2:length(horizon)] %>% strsplit(",") %>%
    sapply(as.numeric) %>% t %>% data.frame
  colnames(df) <- horizon[[1]] %>% strsplit(",") %>% unlist

  # Create directory and save file
  path <- "data/horizon"
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  file_horizon <- sprintf("%s/horizon_%s.csv",path,names(dem))
  write.csv(df,file_horizon,row.names=F)

  # Reformat start point and add end point to plot as polygon
  if(f.plot.polygon){
    ymin_polygon <- -20
    df[1,] <- c(360, ymin_polygon)
    df     <- rbind(df,c(0,ymin_polygon))
  }

  # Clean up
  unlink_.gislock()
  remove_GISRC()
  unlink(gisDbase)
  return(df)
}
