#' Plot horizon with sun positions for a given station location
#'
#' Plot sun position and horizons from local terrain model, local surface model
#' and regional digital terrain model with infos from the weather station.
#'
#' @references \url{https://frost-beta.met.no/docs/codeexamples}
#'
#' @param stn A SpatVector with station attributes from
#'        \code{"get_latlon_frost"}
#' @param dem A SpatRaster of a digital terrain model around the station
#' @param dsm A SpatRaster of a digital surface model around the station
#' @param demkm A SpatRaster of a wider digital terrain model around the station
#' @param path A directory path defining where will be saved the plots,
#'        if path is NULL the plots are printed to the console
#'
#' @return Sun diagram with station metadata
#'
#' @examples
#' # Load the station metadata and location
#' stn <- get_metadata_frost(stationid = 18700, paramid = 211)
#'
#' # Load DEM data
#' dem   <- download_dem_kartverket(stn, name = "dtm", dx = 100, resx = 1)
#' dsm   <- download_dem_kartverket(stn, name = "dom", dx = 100, resx = 1)
#' demkm <- download_dem_kartverket(stn, name = "dtm", dx = 20e3, resx = 20)
#'
#' # Plot sun diagram and save
#' path <- 'plot/horizon'
#' plot_station_horizon_sun(stn, dem, dsm, demkm, path = path)
#'
#' @importFrom terra crds project
#' @import ggplot2
#'
#' @export

plot_station_horizon_sun <- function(stn = NULL,
                                     dem = NULL,
                                     dsm = NULL,
                                     demkm = NULL,
                                     path = NULL) {

  # Extract timezone from System and assign variables
  tz <- Sys.timezone()
  azimuth <- day <- horizon_height <- hour <- inclination <- NULL
  ymax <- 80

  # Extract station name, latlon and level
  stn_name    <- str_to_title(stn$station.name)
  stn_id      <- stn$stationid
  stn_wmoid   <- stn$WMO
  stn_level   <- stn$level
  stn_centre  <- terra::crds(stn)
  stn_latlon  <- terra::crds(terra::project(stn,"epsg:4326"))
  stn_param   <- stn$parameterid
  stn_expos   <- stn$exposure.value
  stn_perf    <- stn$performance.value
  if (stn_expos == "unknown") {
    stn_expos <- NA
    }
  if (stn_perf  == "unknown") {
    stn_perf  <- NA
    }

  # Set cardinals (North position is edited to be visible on the plot)
  cardinals <- data.frame(azimuth = c(0 + 15, 90, 180, 260, 360 - 15),
                          inclination = rep((ymax - 0.05 * ymax), 5),
                          labels = c("North", "East", "South", "West", "North"))

  # Compute sun position from station location
  sun       <- compute_sun_position(stn)
  sun_hour  <- compute_sun_position(stn, f_hour = TRUE)

  # Compute horizon view from location
  step <- 0.01
  horizon_dem   <- compute_horizon(stn_centre,
                                   dem,
                                   level = stn_level,
                                   step = step,
                                   f_plot_polygon = TRUE)
  horizon_dsm   <- compute_horizon(stn_centre,
                                   dsm,
                                   level = stn_level,
                                   step = step,
                                   f_plot_polygon = TRUE)
  horizon_demkm <- compute_horizon(stn_centre,
                                   demkm,
                                   level = stn_level,
                                   step = step,
                                   f_plot_polygon = TRUE)
  horizon_max   <- data.frame(azimuth = horizon_dem[, 1],
                              horizon_height = apply(cbind(horizon_dem[, 2],
                                                           horizon_dsm[, 2],
                                                           horizon_demkm[, 2]),
                                                     1, max))
  skyviewfactor <- compute_skyviewfactor(horizon_max)

  # Plot init
  g <- ggplot()

  # Plot background
  g <- g +
    geom_hline(yintercept = c(0, 7, 20),
               linewidth = .2,
               color = "coral") +
    geom_text(data = cardinals,
              mapping = aes(x = azimuth,
                            y = inclination,
                            label = labels))

  # Plot horizon polygon
  g <- g +
    geom_polygon(data = horizon_max,
                 mapping = aes(x = azimuth,
                               y = horizon_height),
                 alpha = .6,
                 fill = "gray")

  # Plot horizon lines
  g <- g +
    geom_line(data = horizon_max,
              mapping = aes(x = azimuth,
                            y = horizon_height),
              linewidth = .6,
              colour = "gray50") +
    geom_line(data = horizon_dem,
              mapping = aes(x = azimuth,
                            y = horizon_height),
              colour = "gray",
              alpha = .7) +
    geom_line(data = horizon_dsm,
              mapping = aes(x = azimuth,
                            y = horizon_height,
                            linetype = "surface"),
              linewidth = .25) +
    geom_line(data = horizon_demkm,
              mapping = aes(x = azimuth,
                            y = horizon_height,
                            linetype = "horizon"))

  # Plot sun position from the station location
  g <- g +
    geom_path(data = sun_hour,
              aes(x = azimuth,
                  y = inclination,
                  group = hour),
              linewidth = .2,
              color = "coral") +
    geom_line(data = sun,
              aes(x = azimuth,
                  y = inclination,
                  color = day)) +
    scale_color_viridis_d(labels = c("21 jun.",
                                     "21 jul.",
                                     "21 aug.",
                                     "21 sep.",
                                     "21 oct.",
                                     "21 nov.",
                                     "21 dec."))

  # Set theme and legend
  g <- g +
    theme_minimal() +
    theme(legend.position = c(0.9, (ymax - 0.1 * ymax) / ymax),
          legend.justification = c("center", "top"),
          legend.background = element_rect(fill = "white",  linewidth = .2),
          legend.key.size = unit(1.2, "lines"),
          legend.key.height = unit(.8, "lines"),
          legend.margin = margin(.5, 2, 2, 3),
          legend.spacing.y = unit(0, "lines"),
          legend.text = element_text(size = 8)) +
    scale_linetype_manual(values = c("horizon" = "dashed",
                                     "surface" = "solid")) +
    labs(color = NULL, linetype = NULL) +
    xlab(label = "Azimuth (degrees)") +
    ylab(label = "Horizon height (degrees)")

  # Set axis breaks, limits and labels
  xmin <- 0
  xmax <- 360
  g <- g +
    scale_x_continuous(breaks = seq(xmin, xmax, by = 30), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 65, by = 5), expand = c(0, 0)) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(0, ymax), expand = TRUE)

  # Add annotation with infos
  label <- sprintf("Norwegian Meteorological Institute\n")
  label <- sprintf("%slat: %02.2f - long: %02.2f - elev: %1.0f m\n",
                   label,
                   stn_latlon[1],
                   stn_latlon[2],
                   stn$elev)
  label <- sprintf("%sparamid: %i - exp.: %s - perf.: %s\n",
                   label,
                   stn_param,
                   stn_expos,
                   stn_perf)
  label <- sprintf("%stime_zone: %s+1 - svf: %02.2f\n",
                   label,
                   tz,
                   skyviewfactor)
  label <- sprintf("%sstation_id: %i - level: %i\n",
                   label,
                   stn_id,
                   stn_level)
  label <- sprintf("%swmo_id: %s\n%s",
                   label,
                   stn_wmoid,
                   stn_name)
  g <- g +
    annotate("label", x = 10, y = (ymax - 0.1 * ymax), size = 3,
             hjust = 0, vjust = 1, label = label, label.r = unit(0, "pt"))

  # Save plot
  if (!is.null(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    fname <- sprintf("%s/%i_sun_diagram_auto.png", path, stn$stationid)
    ggsave(fname, bg = "white", width = 7, height = 7)
  }

  return(g)
}
