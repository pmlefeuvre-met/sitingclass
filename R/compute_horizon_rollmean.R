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
#' @importFrom stats median approx
#'
#' @export

compute_horizon_rollmean <- function(stn = NULL,
                                     horizon = NULL) {

  # Get sun position for each hour and for summer solstice (highest sun)
  sun <- compute_sun_position(stn, f_hour = TRUE)
  sun_day <- compute_sun_position(stn)
  sun_day <- sun_day[sun_day[, "day"] == "21 juni", 1:2]

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

  # Compute azimuth median for a theoretical hour at midday (add 30% padding)
  azimuth_1hour <- stats::median(sun_hour)
  azimuth_1hour <- round(azimuth_1hour + azimuth_1hour*0.30)

  # Compute azimuth interval from the horizon data
  azimuth_interval <- abs(stats::median(diff(horizon[, "azimuth"])))

  # Compute azimuth window for an hour normalised by the azimuth interval
  window <- azimuth_1hour / azimuth_interval

  # Compute hourly rolling mean based on azimuth
  horizon_mean <- zoo::rollmean(horizon[, "horizon_height"],
                                window,
                                fill = TRUE)

  # Approximate sun inclination to same azimuth interval than horizon array
  sun_day_approx <- stats::approx(x = sun_day[, "azimuth"],
                                  y = sun_day[, "inclination"],
                                  xout = horizon[, "azimuth"],
                                  method = "linear",
                                  rule = 2)$y

  # Remove sun inclinations below horizon
  sun_day_approx <- sun_day_approx * as.numeric(sun_day_approx>0)

  # Remove horizon above and beyond the sun inclination that do not impact
  # temperature sensor like from, for instance, theoretical northern shadows
  # AND Reassign horizon to max sun inclinations
  horizon_mean <-  horizon_mean * (horizon_mean <= sun_day_approx) +
    sun_day_approx * (horizon_mean > sun_day_approx)

  # Return horizon mean
  return(horizon_mean)
}
