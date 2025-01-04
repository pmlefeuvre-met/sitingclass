#' Apply Hourly Rolling Mean to Horizon Height
#'
#' Compute azimuth interval for a theoretical hour based on sun position and
#' apply a rolling mean with this computed window size on horizon height.
#'
#' @references \url{https://github.com/adokter/suntools/}
#'
#' @param stn A SpatVector with station attributes from
#'        \code{"get_latlon_frost"}
#' @param horizon A data.frame with horizon distribution from
#' \code{"compute_horizon_max"}
#'
#' @return an array of horizon height in degrees
#'
#' @examples
#' # Load the station metadata including location and level
#' stn <- get_metadata_frost(stationid = 18700, paramid = 211)
#'
#' # Compute maximum horizon
#' horizon <- compute_horizon_max(stn, step = 1, f_plot_polygon = FALSE)
#'
#' # Apply rolling mean to horizon height with a computed hourly window
#' horizon["horizon_mean"] <- compute_horizon_rollmean(stn, horizon)
#'
#' @importFrom zoo rollmean
#'
#' @export

compute_horizon_rollmean <- function(stn = NULL,
                                     horizon = NULL) {

  # Get sun position for each hour
  sun <- compute_sun_position(stn, f_hour = TRUE)

  # Remove inclinations that are below horizon and above max inclination
  # for class 4, extracting inlinations relevant for siting class.
  sun[,1:2] <- sun[, 1:2] * as.numeric(sun[, "inclination"] > 0 &
                                       sun[, "inclination"] <= 20)

  # Compute the difference of the azimuth at 12:00 and 13:00
  sun_hour <- sun[sun[, "hour"] == 13, "azimuth"] -
              sun[sun[, "hour"] == 12, "azimuth"]

  # Remove difference errors due to removed inclinations,
  # expecting 10-25 degrees in azimuth for an hour
  sun_hour <- sun_hour[sun_hour > 0 & sun_hour < 60]

  # Compute azimuth median for a theoretical hour at midday
  azimuth_1hour <- round(median(sun_hour))

  # Compute azimuth interval from the horizon data
  azimuth_interval <- abs(median(diff(horizon[, "azimuth"])))

  # Compute azimuth window for an hour normalised by the azimuth interval
  window <- azimuth_1hour / azimuth_interval

  # Compute hourly rolling mean based on azimuth
  horizon_mean <- zoo::rollmean(horizon[, "horizon_height"],
                                window,
                                fill = TRUE)

  # Return horizon mean
  return(horizon_mean)
}
