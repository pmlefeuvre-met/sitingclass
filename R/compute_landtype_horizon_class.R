#' Pipeline to compute siting class
#'
#' The pipeline derives the exposure of a station based WMO/met.no siting
#' classification and includes four input functions:
#'\code{"download_dem_kartverket"}, \code{"compute_landtype"},
#' \code{"compute_landtype_distance"} and \code{"compute_horizon_max"}.
#' They provide the necessary input for \code{"compute_class"}
#'
#' @param stn A SpatVector with station attribute `stationid` from
#'        \code{"get_latlon_frost"}
#'
#' @return None
#'
#' @examples
#' # Pipeline to compute necessary input for deriving temperature siting class
#' # compute_landtype_horizon_class(stn)
#'
#' @export

compute_landtype_horizon_class <- function(stn) {

  # Load a digital elevation model
  dem <- download_dem_kartverket(stn, name = "dtm")

  # Compute land type
  landtype <- compute_landtype(stn,
                               f_plot = TRUE)

  # Compute land type distance to station
  landtype_dist <- compute_landtype_distance(stn,
                                             landtype,
                                             f_plot = TRUE)
  # Compute maximum horizon
  horizon_max <- compute_horizon_max(stn,
                                     step = 0.01,
                                     f_plot_polygon = FALSE)

  # Compute class
  class <- compute_class(stn,
                         landtype_dist,
                         horizon_max,
                         dem,
                         test_type = "WMO",
                         f_plot = TRUE)

  return(class)
}
