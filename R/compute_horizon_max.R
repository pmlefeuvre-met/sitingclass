#' Maximum horizon height around a station
#'
#' Compute maximum horizon height in degrees from a station location in UTM 33
#' and based on three DEMs: Digital Terrain Model, Digital Surface Model and
#' 20km-Digital Terrain Model
#'
#' @references \url{https://grass.osgeo.org/grass83/manuals/r.horizon.html}
#'
#' @param stn A SpatVector with station attributes from
#' \code{"get_latlon_frost"}
#' @param dx A distance in metre or radius defining the extent of the
#'        bounding box from the centre point
#' @param resx A horizontal resolution in metre
#' @param step An interval in degrees at which the horizon will be computed,
#'        default is every 10 deg.
#' @param f_plot_polygon A boolean setting boundary values to -20 deg to
#'        facilitate plotting as polygon
#'
#' @return A dataframe with `horizon_height` in degrees and `azimuth` angle in
#'        degrees at which the horizon is computed in degrees
#'
#' @importFrom terra crds
#'
#' @examples
#' # Load the station metadata including location and level
#' stn <- get_latlon_frost(stationid = 18700, paramid = 211)
#'
#' # Parameters
#' dx <- 100
#' resx <- 1
#'
#' # Compute the maximum horizon
#' compute_horizon_max(stn, dx, resx)
#' compute_horizon_max(stn, dx, resx, step = 1, f_plot_polygon = FALSE)
#'
#' @export

compute_horizon_max <- function(stn = NULL,
                                dx = 100,
                                resx = 1,
                                step = 10,
                                f_plot_polygon = FALSE) {
  # Get station metadata
  stn_centre  <- terra::crds(stn)
  stn_level   <- stn$id.level

  # Load digital elevation models of the terrain and surface
  dem   <- download_dem_kartverket(stn, name = "dtm", dx, resx)
  dsm   <- download_dem_kartverket(stn, name = "dom", dx, resx)
  demkm <- download_dem_kartverket(stn, name = "dtm", dx = 20e3, resx = 20)

  # Compute horizon for three DEMs: DTM, DSM and DTM_20km
  horizon_dem   <- compute_horizon(stn_centre,
                                   dem,
                                   level = stn_level,
                                   step = step,
                                   f_plot_polygon = f_plot_polygon)
  horizon_dsm   <- compute_horizon(stn_centre,
                                   dsm,
                                   level = stn_level,
                                   step = step,
                                   f_plot_polygon = f_plot_polygon)
  horizon_demkm <- compute_horizon(stn_centre,
                                   demkm,
                                   level = stn_level,
                                   step = step,
                                   f_plot_polygon = f_plot_polygon)

  # Compute highest horizon
  horizon_max   <- data.frame(azimuth = horizon_dem[, 1],
                              horizon_height = apply(cbind(horizon_dem[, 2],
                                                           horizon_dsm[, 2],
                                                           horizon_demkm[, 2]),
                                                     1, max))

  # Return output
  return(horizon_max)
}
