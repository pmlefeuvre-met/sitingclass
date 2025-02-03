#' Plot max horizon and range from the station
#'
#' Plot maximum horizon and its range based on the local terrain model, the
#' local surface model and the regional digital terrain model with infos
#' from the weather station.
#'
#' @references \url{https://frost-beta.met.no/docs/codeexamples}
#'
#' @param stn A SpatVector with station attributes from
#'        \code{"get_metadata_frost"}
#' @param path A directory path defining where will be saved the plots,
#'        if path is NULL the plots are printed to the console
#'
#' @return A figure
#'
#' @examples
#' # Load the station metadata and location
#' stn <- get_metadata_frost(stationid = 18700, path = "plot/horizon")
#'
#' # Plot sun diagram and save
#' plot_station_horizon_range(stn)
#'
#' @importFrom terra crds project
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales pseudo_log_trans rescale
#' @importFrom ggplot2 ggplot geom_hline geom_text geom_polygon geom_line
#' @importFrom ggplot2 geom_path scale_color_viridis_d theme_minimal theme
#' @importFrom ggplot2 labs xlab ylab scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 coord_cartesian annotate ggsave
#'
#' @export

plot_station_horizon_range <- function(stn = NULL,
                                       path = stn$path) {

  # Extract timezone from System and assign variables
  azimuth <- day <- horizon_height <- hour <- inclination <- NULL
  tz <- "UTC" #Sys.timezone()
  ymax <- 80

  # Extract station name and coordinates
  stn_name    <- str_to_title(stn$station.name)
  stn_lonlat  <- terra::crds(terra::project(stn, "epsg:4326"))

  # Set cardinals (North position is edited to be visible on the plot)
  cardinals <- data.frame(azimuth = c(0 + 15, 90, 180, 260, 360 - 15),
                          inclination = rep((ymax - 0.05 * ymax), 5),
                          labels = c("North", "East", "South", "West", "North"))

  # Compute horizon view from location
  step <- 1
  f_plot_polygon <- TRUE
  horizons <- compute_horizon_max(stn,
                                  step = step,
                                  f_plot_polygon = f_plot_polygon,
                                  f_output_all = TRUE)

  # Reassign output
  horizon_max   <- data.frame(azimuth = horizons[, "azimuth"],
                              horizon_height = horizons[, "horizon_max"],
                              range = horizons[, "range_max"])

  # Compute horizon view from location
  skyviewfactor <- compute_skyviewfactor(horizon_max)

  ## Plot parameters for scale_fill_gradientn():
  # 1. Colour palette for horizon range changing at 100 m
  colours <- c(rev(RColorBrewer::brewer.pal(6, "Paired")),
               RColorBrewer::brewer.pal(4, "Purples"))
  values <- c(0, 5, 10, 30, 50, 70, 90, 100, 1000, 5000, 20000)

  # 2. Log transform function for scale_fill and legend
  trans <- scales::pseudo_log_trans()

  ## Plot
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
    geom_bar(stat="identity",
             data =  horizon_max,
             mapping = aes(x = azimuth,
                           y = horizon_height,
                           fill = range),
             alpha = .6) +
    scale_fill_gradientn(colours = colours,
                         values = scales::rescale(trans$transform(values)),
                         breaks = c(0, 3, 10, 30, 100, 1000, 5000, 20000),
                         limits = c(0, 20000),
                         transform = trans)


  # Plot horizon lines
  g <- g +
    geom_line(data = horizon_max,
              mapping = aes(x = azimuth,
                            y = horizon_height),
              linewidth = .6,
              colour = "gray50")

  # Set theme and legend
  g <- g +
    theme_minimal() +
    theme(legend.position = "inside",
          legend.position.inside =  c(0.9, (ymax - 0.1 * ymax) / ymax),
          legend.justification = c("center", "top"),
          legend.background = element_rect(fill = "white",  linewidth = .2),
          legend.key.size = unit(1.2, "lines"),
          legend.key.height = unit(2, "lines"),
          legend.margin = margin(5, 5, 8, 5),
          legend.spacing.y = unit(0, "lines"),
          legend.text = element_text(size = 8)) +
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
                   stn_lonlat[2],
                   stn_lonlat[1],
                   stn$elev)
  label <- sprintf("%stime_zone: %s+1 - svf: %02.2f\n",
                   label,
                   tz,
                   skyviewfactor)
  label <- sprintf("%sstation: %s",
                   label,
                   stn_name)

  g <- g +
    annotate("label", x = 10, y = (ymax - 0.1 * ymax), size = 3,
             hjust = 0, vjust = 1, label = label, label.r = unit(0, "pt"))

  # Save plot
  if (!is.null(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    fname <- sprintf("%s/%1.0f_sun_diagram_auto_range.png", path, stn$stationid)
    ggsave(fname, bg = "white", width = 10, height = 7)
  }

  return(g)
}
