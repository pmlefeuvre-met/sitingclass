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
#' compute_horizon_max(stn, step = 1, f_output_all = TRUE)
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
  demkm <- download_dem_kartverket(stn, name = "dtm", dx = 20e3, resx = 20)

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

  # Combine horizon output for processing
  horizon_matrix <- cbind(horizon_dem[, "horizon_height"],
                          horizon_dsm[, "horizon_height"],
                          horizon_demkm[, "horizon_height"])

  # Compute highest horizon (default output)
  horizon_max   <- apply(horizon_matrix, 1, max)

  if ("horizon_distance" %in% names(horizon_dem)){
    # Combine distance/range output for processing
    range_matrix <- cbind(horizon_dem[, "horizon_distance"],
                          horizon_dsm[, "horizon_distance"],
                          horizon_demkm[, "horizon_distance"])

    # Extract the column with the highest horizon height per row
    max_col_index <- max.col(m = horizon_matrix, ties.method = 'first')

    # Extract the distance/range with the highest horizon per row
    range_max <- sapply(1:length(max_col_index),
                        function(i) range_matrix[i, max_col_index[i]])
  }

  # Produced the combined output as data.frame
  horizon_max   <- data.frame(azimuth = horizon_dem[, "azimuth"],
                              horizon_height = horizon_max,
                              range = range_max)

  if(!f_output_all){
    # Return simplified output
    return(horizon_max)

  }else{
    # Construct a dataframe with all output data
    # Combine all horizon
    horizons <- data.frame(azimuth = horizon_dem[, "azimuth"],
                           horizon_max = horizon_max[, "horizon_height"],
                           horizon_dem = horizon_dem[, "horizon_height"],
                           horizon_dsm = horizon_dsm[, "horizon_height"],
                           horizon_demkm = horizon_demkm[, "horizon_height"])

    # If distance is computed (with grass84+)
    if ("horizon_distance" %in% names(horizon_dem)){
      # Then Add Distance that is renamed range to refer to the line of sight
      ranges <- data.frame(range_max = horizon_max[, "range"],
                           range_dem = horizon_dem[, "horizon_distance"],
                           range_dsm = horizon_dsm[, "horizon_distance"],
                           range_demkm = horizon_demkm[,"horizon_distance"])

      # Combine horizon and ranges
      horizons <- cbind(horizons, ranges)
    }

    # Return output
    return(horizons)
  }
}
