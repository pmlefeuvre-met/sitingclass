#' Add grid/buffer
#'
#' Add a grid and add buffers on a map to assess the area of same land cover
#' type.
#'
#' @param g A ggplot objects on which to add the grid
#' @param centre The centre coordinates (lat, lon)
#' @param nx A number defining the grid interval in metre for x and y
#' @param n A number of grid segments from one side of the centre point
#'
#' @return A ggplot2 object
#'
#' @examples
#' # Get station metadata
#' stn <- get_metadata_frost(stationid = 18700)
#'
#' # Get coordinates and define boundary box
#' centre <- terra::crds(stn)
#' box <- make_bbox(centre, dx = 1600)
#'
#' # Plot map tile
#' g <- plot_tile_station(stn, box, tile_name = "ortofoto")
#'
#' # Add grid and buffer
#' nx <- 200
#' g <- add_grid(g, centre, nx)
#' g <- add_buffer(g, centre, 300, 1000, nx)
#' g
#'
#' @importFrom ggplot2 geom_segment geom_label
#'
#' @export
add_grid <- function(g = NULL,
                     centre = NULL,
                     nx = NULL,
                     n = 6) {

  # Bind variables to function
  xend <- yend <- NULL

  # Set the interval of the horizontal (y) and vertical lines
  x <- seq(centre[1] - nx * n, centre[1] + nx * n, by = nx)
  y <- seq(centre[2] - nx * n, centre[2] + nx * n, by = nx)

  # Set the segment start and end points as data.frame
  dfx <- data.frame(x = x,
                    xend = x,
                    y = min(y),
                    yend = max(y),
                    row.names = NULL)

  dfy <- data.frame(x = min(x),
                    xend = max(x),
                    y = y,
                    yend = y,
                    row.names = NULL)

  # Plot the grid as an array of segments
  g <- g +
    geom_segment(data = dfx,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = .5,
                 color = "gray90") +
    geom_segment(data = dfy,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = .5,
                 color = "gray90")

  # Add label as scale
  g <- g +
    geom_label(aes(label = sprintf("%i m", nx),
                   x = max(x) - nx / 2,
                   y = min(y) - nx / 2),
               size = 2)

  return(g)
}
