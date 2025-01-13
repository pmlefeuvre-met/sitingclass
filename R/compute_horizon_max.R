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
#' @param step An interval in degrees at which the horizon will be computed,
#'        default is every 10 deg.
#' @param f_plot_polygon A boolean setting boundary values to -20 deg to
#'        facilitate plotting as polygon
#' @param f_output_all A boolean to export all horizons: dem, dsm, demkm & max
#'
#' @return A dataframe with `horizon_height` in degrees and `azimuth` angle in
#'        degrees at which the horizon is computed in degrees
#'
#' @importFrom terra crds
#'
#' @examples
#' # Load the station metadata including location and level
#' stn <- get_metadata_frost(stationid = 18700, dx = 100, resx = 1)
#'
#' # Compute the maximum horizon
#' compute_horizon_max(stn)
#' compute_horizon_max(stn, step = 1, f_plot_polygon = FALSE)
#'
#' @export

compute_horizon_max <- function(stn = NULL,
                                step = 10,
                                f_plot_polygon = FALSE,
                                f_output_all = FALSE) {
  # Get station metadata
  stn_level   <- stn$level

  # Load digital elevation models of the terrain and surface
  dem   <- download_dem_kartverket(stn, name = "dtm")
  dsm   <- download_dem_kartverket(stn, name = "dom")
  demkm <- download_dem_kartverket(stn, name = "dtm", dx = 1e3, resx = 1)

  # Compute horizon for three DEMs: DTM, DSM and DTM_20km
  horizon_dem   <- compute_horizon(stn,
                                   dem,
                                   level = stn_level,
                                   step = step,
                                   f_plot_polygon = f_plot_polygon)
  Sys.sleep(0.1)
  horizon_dsm   <- compute_horizon(stn,
                                   dsm,
                                   level = stn_level,
                                   step = step,
                                   f_plot_polygon = f_plot_polygon)
  Sys.sleep(0.1)
  horizon_demkm <- compute_horizon(stn,
                                   demkm,
                                   level = stn_level,
                                   step = step,
                                   f_plot_polygon = f_plot_polygon)
  Sys.sleep(0.1)

  # Compute highest horizon
  horizon_max   <- data.frame(azimuth = horizon_dem[, 1],
                              horizon_height = apply(cbind(horizon_dem[, 2],
                                                           horizon_dsm[, 2],
                                                           horizon_demkm[, 2]),
                                                     1, max))
  if(!f_output_all){
    # Return output
    return(horizon_max)
  }else{
    # Combine
    horizons <- data.frame(azimuth = horizon_dem[, 1],
                           horizon_max = horizon_max[, 2],
                           horizon_dem = horizon_dem[, 2],
                           horizon_dsm = horizon_dsm[, 2],
                           horizon_demkm = horizon_demkm[, 2])

    # Return output
    return(horizons)
  }
}
