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
#' # Get station metadata
#' stn <- get_metadata_frost(stationid = 73466, dx = 100, resx = 1)
#'
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
  step=.01
  horizon_max <- compute_horizon_max(stn,
                                     step = step,
                                     f_output_all = TRUE)

  # Keep only obstacles above mountains
  horizon_obstacles <- horizon_max[c("azimuth","horizon_dsm","range_dem")]
  horizon_obstacles["horizon_dsm"] <- horizon_obstacles["horizon_dsm"] *
    (horizon_max$horizon_dsm > horizon_max$horizon_demkm)
  names(horizon_obstacles) <-  c("azimuth", "horizon_height", "range")

  horizon_obstacles[,"dhorizon"] <- c(0,diff(horizon_obstacles[,"horizon_height"])/step)
  dh_threshold <-  2/step
  n_pos <- which(horizon_obstacles["dhorizon"]>=dh_threshold)
  n_neg <- which(horizon_obstacles["dhorizon"]<=-dh_threshold)
  n_range <- (n_pos[length(n_pos)]-1/step):(n_neg[1]+1/step)
  if(diff(range(n_range))*step < 25){
    horizon_obstacles[,"horizon_height"] <- 0 #index of horizon is reversed
  }
  g0 <- ggplot(data = horizon_max, aes(x=azimuth, y=horizon_dsm)) +
    geom_line() + theme_minimal()

  g1 <- ggplot(data = horizon_obstacles, aes(x=azimuth, y=horizon_height)) +
    geom_line() + theme_minimal()

  g2 <- ggplot(data = horizon_obstacles, aes(x=azimuth, y=dhorizon))+
    geom_line() + theme_minimal() + geom_hline(yintercept = c(dh_threshold,-dh_threshold),colour="grey") +
    geom_point(data=horizon_obstacles[n_pos,], aes(x=azimuth,y=dhorizon),colour="coral") +
    geom_point(data=horizon_obstacles[n_neg,], aes(x=azimuth,y=dhorizon),colour="skyblue")
  g0/g1/g2

  # unique(floor(horizon_max$azimuth[n_neg]))
  # unique(ceiling(horizon_max$azimuth[n_pos]))
  # c(sort(horizon_max$azimuth[n_neg])[1],sort(horizon_max$azimuth[n_pos])[length(n_neg)])
  # c(n_neg[1],n_pos[length(n_neg)])

  # DETERMINE PERCENTAGE ABOVE HORIZON TO MAKE ALGO KINDER 44300 SÆRHEIM

  # Compute class
  class <- compute_class_air_temperature(stn,
                                         landtype_dist,
                                         horizon_obstacles,
                                         dem,
                                         test_type = "WMO",
                                         f_plot = f_plot)

  return(class)
}
