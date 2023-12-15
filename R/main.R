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

# Station ID
for (stationid in stationid_array){
#  try(plot_station_siting_context(stationid,paramid=211,f.verbose=F,f.pdf=F))
}
