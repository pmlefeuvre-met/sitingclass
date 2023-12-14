#' Sun position in the sky
#'
#' Compute sun position in the sky from a station location in UTM 33
#'
#' @references \url{https://github.com/adokter/suntools/}
#'
#' @param stn A SpatVector with station attributes from \code{"get_latlon_frost"}
#' @param f.hour A boolean value to compute sun position for hours if TRUE and for days if FALSE
#'
#' @return dataframe with inclination of the sun position in the sky in degrees, azimuth in degrees and timestamp as factor
#'
#' @examples
#' compute_sun_position(stn, f.hour = TRUE)
#' compute_sun_position(stn, f.hour = FALSE)
#'
#'
#' @export

compute_sun_position <- function(stn = NULL,
                                 f.hour = F){

  # Convert back to latlon
  latlon <- stn %>% sf::st_transform(4326) %>% sf::st_coordinates
  # Extract timezone from System
  tz <- Sys.timezone()

  if(f.hour){
    # Function to get sun position at the same hour over six months, on each 21st of the month
    seq_month_hour <- function(h){
      seq(as.POSIXct(sprintf("2023-06-21 %02.0f:00:00",h), tz = tz),
          as.POSIXct(sprintf("2023-12-21 %02.0f:59:00",h), tz = tz),
          "1 days")}

    # Extract day sequence for specific month
    seq_hour <- seq(0, 23)

    # Concatenate input time per hours over six months
    sun_hours <- do.call("c", lapply(seq_hour, seq_month_hour) )

    # Compute sun position
    sun_year_hour <- suntools::solarpos(matrix(latlon[1:2], nrow = 1), sun_hours)

    # Convert output to dataframe
    df  <- data.frame(azimuth = sun_year_hour[,1],
                      inclination = sun_year_hour[,2],
                      hour = factor(as.numeric(format(sun_hours, '%H', tz = tz)),
                                    sort(as.numeric(unique(format(sun_hours, '%H', tz = tz))))
                      ))
  }else{
    # Function to get day sequence in minutes, on each 21st of the month
    seq_day <- function(m){
      seq(as.POSIXct(sprintf("2023-%02.0f-21 00:00:00",m), tz = tz),
          as.POSIXct(sprintf("2023-%02.0f-21 23:59:00",m), tz = tz),
          "mins")}

    # Extract day sequence for specific month
    seq_month <- seq(6,12)

    # Concatenate input time per days of the month
    sun_days <- do.call("c", lapply(seq_month, seq_day) )

    # Compute sun position
    sun_year <- suntools::solarpos(matrix(latlon[1:2], nrow = 1), sun_days)

    # Convert output to dataframe
    df  <- data.frame(azimuth = sun_year[,1],
                      inclination = sun_year[,2],
                      day = factor(format(sun_days, '%d %b'),
                                   unique(format(sun_days, '%d %b'))
                      ))
  }

  return(df)
}
