#' Horizon height at a station
#'
#' Compute horizon height in degrees from a station location in UTM 33
#'
#' @references \url{https://grass.osgeo.org/grass83/manuals/r.horizon.html}
#'
#' @param stn A SpatVector or an array of two coordinates in UTM 33
#' @param dem A SpatRaster of a digital elevation/surface model in
#'        UTM 33 (epsg:25833)
#' @param level A height above the ground of the sensor in metres,
#'        level 0 is set to 2 metres
#' @param step An interval in degrees at which the horizon will be computed,
#'        default is every 10 deg.
#' @param f_plot_polygon A boolean setting boundary values to -20 deg to
#'        facilitate plotting as polygon
#'
#' @return A dataframe with `horizon_height` in degrees and `azimuth` angle in
#'        degrees at which the horizon is computed in degrees
#'
#' @importFrom terra crds cellFromXY
#' @importFrom rgrass initGRASS write_RAST execGRASS
#' @importFrom rgrass unlink_.gislock remove_GISRC
#' @importFrom utils write.csv
#'
#' @examples
#' # Load the station metadata including location and level
#' stn <- get_metadata_frost(stationid = 18700, paramid = 211)
#'
#' # Load a digital elevation model
#' dsm   <- download_dem_kartverket(stn, name = "dom", dx = 100, resx = 1)
#'
#' # Compute the horizon
#' compute_horizon(stn, dsm)
#' compute_horizon(stn, dsm, step = 1, f_plot_polygon = TRUE)
#'
#' @export

compute_horizon <- function(stn = NULL,
                            dem = NULL,
                            level = NULL,
                            step = 10,
                            f_plot_polygon = FALSE) {

  # Get centre and level
  if (!is.matrix(stn)) {
    if (terra::is.valid(stn)) {
      centre <- terra::crds(stn)
      if (is.null(level)) {
        level <- stn$level
      }
    }
  } else {
    centre <- stn
  }
  centre <- round(centre, 2)


  if (is.null(level)) {
    level <- 0
  }

  # Adjust ground level to match real sensor height
  level <- ifelse(level == 0, 2, level)
  loc <-  terra::cellFromXY(dem, centre)
  dem[c(loc)] <- dem[c(loc)] + level

  # Set GRASS path
  suppressWarnings({
    grasslib <- try(system("grass --config", intern = TRUE))[4]
    #grasslib <- "/home/pierreml/local/grass85"
  })
  gisDbase <- "/tmp/grassdata/"

  # Initialise GRASS and projection
  rgrass::initGRASS(gisBase = grasslib,
                    home = tempdir(check = TRUE),
                    SG = dem,
                    gisDbase = gisDbase,
                    mapset = "PERMANENT",
                    override = TRUE,
                    remove_GISRC = TRUE)

  # Load DEM
  rgrass::write_RAST(dem, "elev", flags=c("o", "overwrite"), verbose=TRUE)
  # execGRASS("g.list", parameters = list(type = "raster"))
  # Compute horizon
  horizon <- rgrass::execGRASS("r.horizon",
                               flags = c("d", "c", "l", "overwrite"),
                               parameters = list(elevation = "elev",
                                                 coordinates = centre[1:2],
                                                 direction = 90,
                                                 distance = 0.5,
                                                 step = step,
                                                 start = 0,
                                                 end = 360),
                               intern = TRUE)

  # Extract column names
  names <- unlist(strsplit(horizon[[1]], ","))
  columns <- length(names)

  # Construct data frame from GRASS output
  df <- data.frame(t(vapply(strsplit(horizon[2:length(horizon)],
                                     ","),
                            as.numeric,
                            numeric(columns))))

  # Name columns
  colnames(df) <- names

  # Create directory and save file
  if (!is.null(stn$path)) {
    dir.create(stn$path, showWarnings = FALSE, recursive = TRUE)
    file_horizon <- sprintf("%s/horizon_%s.csv", stn$path, names(dem))
    utils::write.csv(df, file_horizon, row.names = FALSE)
  }

  # Reformat start point and add end point to plot as polygon
  if (f_plot_polygon) {
    ymin_polygon <- -20
    # Number of element depends on the number of columns: azimuth, horizon and
    # the newly added column: distance of the horizon
    if(columns == 2){
      df[1, ] <- c(360, ymin_polygon)
      df      <- rbind(df, c(0, ymin_polygon))
    }else if(columns == 3){
      df[1, ] <- c(360, ymin_polygon, 0)
      df      <- rbind(df, c(0, ymin_polygon, 0))
    }
  }

  # Clean up
  rgrass::unlink_.gislock()
  rgrass::remove_GISRC()
  unlink(gisDbase, recursive = TRUE)

  # Return output
  return(df)
}
