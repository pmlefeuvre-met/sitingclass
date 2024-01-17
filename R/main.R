#' Produce sun diagram and maps at the station
#'
#' Run plot_station_siting_context
#'
#' @references \url{https://community.wmo.int/en/activity-areas/
#' imop/siting-classification#MembersTools}
#'
#' @return plots or images of the sun diagram and maps
#'
#' @export
# #stationid <- 18920 #97251 #18703 #18730 #18920
# plot_station_siting_context(stationid=12960,paramid=211,f_verbose=F)

# Defines stations to process
stationid_array <- c(12960, 13390, 17150, 17850, 18700, 18703,
                    18730, 18920, 31410, 50500, 55290, 57780, 88580, 97251)

Sys.setenv(TZ="UTC") # "Europe/Oslo"

# Station ID
# for (stationid in stationid_array){
  # try(plot_station_siting_context(stationid,
  #                                 paramid = 211,
  #                                 f_verbose = F,
  #                                 f_pdf = F))
  # # # Get station metadata
  # stn <- get_metadata_frost(stationid = 18700)
  # # Parameters
  # stn$dx <- 100
  # stn$resx <- 1
  #
  # # Compute land type
  # landtype <- compute_landtype(stn, f_plot = TRUE)
  #
  # # Compute land type distance to station
  # landtype_dist <- compute_landtype_distance(stn,
  #                                            landtype,
  #                                            f_plot = TRUE)
  # # # Compute maximum horizon
  # horizon_max <- compute_horizon_max(stn,
  #                                    step = 1,
  #                                    f_plot_polygon = FALSE)
# }
