#' Produce sun diagram and maps at the station
#'
#' Run plot_station_siting_context
#'
#' @references \url{https://community.wmo.int/en/activity-areas/imop/siting-classification#MembersTools}
#'
#' @return plots or images of the sun diagram and maps
#'
#' @export
# stationid <- 18920 #97251 #18703 #18730 #18920
# plot_station_siting_context(12960,paramid=211,f.verbose=F)

# Defines stations to process
stationid_array <- c( 12960, 13390, 17150, 17850, 18700, 18703, 18730, 18920, 31410, 50500, 55290, 57780, 88580, 97251)

Sys.setenv(TZ="UTC") # "Europe/Oslo"

# Station ID
for (stationid in stationid_array){
#  try(plot_station_siting_context(stationid,paramid=211,f.verbose=F,f.pdf=F))
  # # Get station metadata
  # stn <- get_latlon_frost(stationid=18700)
  # # Parameters
  # dx   <- 100
  # resx <- 1
  # # Compute land type
  # landtype <- compute_landtype(stn, dx, resx, f.plot=TRUE)
  # # Compute land type distance to station
  # landtype_dist <- compute_landtype_distance(stn, landtype, dx, resx, f.plot=TRUE)
  # # Compute maximum horizon
  # horizon_max <- compute_horizon_max(stn, dx, resx, step=1, f.plot.polygon=FALSE)
}
