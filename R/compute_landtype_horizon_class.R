#' Pipeline to compute siting class
#'
#' The pipeline derives the exposure of a station based WMO/met.no siting
#' classification and includes four input functions:
#'\code{"download_dem_kartverket"}, \code{"compute_landtype"},
#' \code{"compute_landtype_distance"} and \code{"compute_horizon_max"}.
#' They provide the necessary input for \code{"compute_class_air_temperature"}.
#'
#' @param stn A SpatVector with station attribute `stationid` from
#'        \code{"get_latlon_frost"}
#' @param f_plot A boolean whether to plot and save figures
#'
#' @return None
#'
#' @examples
#' # Pipeline to compute input for deriving air_temperature siting class
#' # compute_landtype_horizon_class(stn)
#'
#' @export

compute_landtype_horizon_class <- function(stn,
                                           f_plot = FALSE) {

  # Load a digital elevation model
  dem <- download_dem_kartverket(stn, name = "dtm")

  # Compute land type
  landtype <- compute_landtype(stn,
                               f_plot = f_plot)

  # Compute land type distance to station
  landtype_dist <- compute_landtype_distance(stn,
                                             landtype,
                                             f_plot = f_plot)
  # Compute maximum horizon
  horizon_max <- compute_horizon_max(stn,
                                     step = 0.01)

  # Compute class
  class <- compute_class_air_temperature(stn,
                                         landtype_dist,
                                         horizon_max,
                                         dem,
                                         test_type = "WMO",
                                         f_plot = f_plot)

  return(class)
}
