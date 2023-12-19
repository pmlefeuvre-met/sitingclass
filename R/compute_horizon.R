#' Horizon height at a station
#'
#' Compute horizon height in degrees from a station location in UTM 33
#'
#' @references \url{https://grass.osgeo.org/grass83/manuals/r.horizon.html}
#'
#' @param centre An array of two coordinates in UTM 33 (epsg:25833)
#' @param dem A SpatRaster of a digital elevation/surface model in UTM 33 (epsg:25833)
#' @param level A height above the ground of the sensor in metres, level 0 is set to 2 metres
#' @param step An interval in degrees at which the horizon will be computed, default is every 10 deg.
#' @param f.plot.polygon A boolean setting boundary values to -20 deg to facilitate plotting as polygon
#'
#' @return A dataframe with `horizon_height` in degrees and `azimuth` angle in degrees at which the horizon is computed in degrees
#'
#' @importFrom terra cellFromXY
#' @importFrom rgrass initGRASS write_RAST execGRASS unlink_.gislock remove_GISRC
#' @importFrom magrittr %>%
#' @importFrom utils write.csv
#'
#' @examples
#' require(sf)
#'
#' # Load the station metadata including location and level
#' stn <- get_latlon_frost(stationid=18700, paramid=211)
#' stn.id      <- stn$id.stationid
#' stn.centre  <- sf::st_coordinates(stn)
#' stn.level   <- stn$id.level
#'
#' # Load a digital elevation model
#' dsm   <- download_dem_kartverket(stn.id, stn.centre, name="dom", dx=100, resx=1)
#'
#' # Compute the horizon
#' compute_horizon(stn.centre, dsm)
#' compute_horizon(stn.centre, dsm, level=stn.level, step=1, f.plot.polygon=TRUE)
#'
#' @export

compute_horizon <- function(centre = NULL,
                            dem = NULL,
                            level = 2,
                            step = 10,
                            f.plot.polygon = F){

  # Adjust ground level to match real sensor height
  level <- ifelse(level==0, 2, level)
  loc <-  cellFromXY(dem, centre)
  dem[c(loc)] <- dem[c(loc)] + level

  # Set GRASS path
  grasslib <- try(system('grass --config', intern=TRUE))[4]
  gisDbase <- 'data/grassdata/'

  # Initialise GRASS and projection
  initGRASS(gisBase = grasslib,
                    home = tempdir(),
                    SG = dem,
                    gisDbase = gisDbase,
                    mapset = "PERMANENT",
                    override = TRUE,
                    remove_GISRC = TRUE)

  # Load DEM
  write_RAST(dem, "elev", flags="o")

  # Compute horizon
  horizon <- execGRASS("r.horizon",
                               flags=c('d','c','overwrite'),
                               parameters = list(elevation='elev',
                                                 coordinates=round(centre[1:2],2),
                                                 direction=90,
                                                 distance=0.5,
                                                 step=step,
                                                 start=0,
                                                 end=360),
                               intern = T)

  # Construct data frame from GRASS output
  df <- horizon[2:length(horizon)] %>%
    strsplit(",") %>%
    sapply(as.numeric) %>%
    t %>%
    data.frame

  # Name columns
  colnames(df) <- horizon[[1]] %>% strsplit(",") %>% unlist

  # Create directory and save file
  path <- "data/horizon"
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  file_horizon <- sprintf("%s/horizon_%s.csv", path, names(dem))
  write.csv(df, file_horizon, row.names = FALSE)

  # Reformat start point and add end point to plot as polygon
  if(f.plot.polygon){
    ymin_polygon <- -20
    df[1,] <- c(360, ymin_polygon)
    df     <- rbind(df, c(0, ymin_polygon))
  }

  # Clean up
  unlink_.gislock()
  remove_GISRC()
  unlink(gisDbase, recursive = TRUE)

  # Return output
  return(df)
}
