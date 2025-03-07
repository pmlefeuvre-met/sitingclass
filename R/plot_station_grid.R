#' Plot station with Ortophoto, grid and buffers
#'
#' Overlay a grid and two buffers to count the area of land cover types and
#' assess heat source proximity to a weather station and their distribution.
#' Four different scales are plotted: 10 m, 30 m, 100 m and 1000 m.
#'
#' @param stn A SpatVector with station attributes from
#'        \code{"get_latlon_frost"}
#' @param tile_name A string naming a tile to pass to \code{"get_tile_wms"}
#' @param path A directory path defining where will be saved the plots,
#'        if path is NULL the plots are printed to the console
#' @param grid_scale A value or array setting plot extent and grid parameters
#'        that would be looped through if `grid_scale` is an array
#'
#'
#' @return A ggplot2 object
#'
#' @examples
#' # Get station metadata
#' stn    <- get_metadata_frost(stationid = 18700)
#'
#' # Plot grid and buffers on four different scales
#' #plot_station_grid(stn)
#'
#' @importFrom terra crds
#'
#' @export

plot_station_grid <- function(stn = NULL,
                              tile_name = "ortofoto",
                              path = stn$path,
                              grid_scale = c(10, 50, 100, 1000)) {

  # Get coordinates
  centre <- terra::crds(stn)

  # Directory to save plots
  if (!is.null(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }

  for (gscale in grid_scale){
    print(sprintf("grid: %s --- %i-m scale", tile_name, gscale))

    if (gscale == 1000 ) {
      # 1) 1000-metre scale
      dx   <- 1600 # Radius in metre for map extent
      nx   <- 200  # Interval between grid lines in metre
      buf1 <- 300  # Radius in metre for inner circle
      buf2 <- 1000 # Radius in metre for outer circle

    } else if (gscale == 100) {
      # 2) 100-metre scale
      dx   <- 160
      nx   <- 20
      buf1 <- 30
      buf2 <- 100

    } else if (gscale == 50) {
      # 3) 50-metre scale
      dx   <- 50
      nx   <- 5
      buf1 <- 10
      buf2 <- 30

    } else if (gscale == 10) {
      # 4) 10-metre scale with 5- and 10-metre radius
      dx   <- 16
      nx   <- 2
      buf1 <- 3
      buf2 <- 5
    }
    n <- (dx / nx - 2)

    #-----------------------------
    box <- make_bbox(centre, dx)
    g <- plot_tile_station(stn, box, tile_name, path=NULL)

    # Add grid and buffer. dx and nx are in metre.
    g <- add_grid(g, centre, nx = nx, n = n)
    if (gscale == 10) {g <- add_buffer(g, centre, buf1 = 10, nx = nx, n = n)}
    g <- add_buffer(g, centre, buf1 = buf1, buf2 = buf2, nx = nx, n = n)

    # Remove title and axis
    g <- g + theme_void() + labs(title = NULL, subtitle = NULL)

    #-----------------------------
    # Save plot
    if (!is.null(path)) {
      fname <- sprintf("%s/%1.0f_map_grid_%s_%04.0fm.png", path,
                       stn$stationid, tile_name, dx)
      ggsave(fname, bg = "white", width = 7, height = 7)
    } else {
      print(g)
    }

    # END LOOP
  }
  return(g)
  # END FUNCTION
}
