#' Add grid/buffer
#'
#' Add a grid and add buffers on a map to assess the area of same land cover
#' type.
#'
#' @param g A ggplot objects on which to add the grid
#' @param box A SpatExtent to get the box extent
#' @param nx A number defining the grid interval in metre for x and y
#' @param n A number to set the border of the grid from the edge of the plot
#'
#' @return A ggplot2 object
#'
#' @examples
#' # Get station metadata
#' stn <- get_latlon_frost(stationid = 18700)
#'
#' # Get coordinates and define boundary box
#' centre <- sf::st_coordinates(stn)
#' box <- make_bbox(centre, dx = 1600)
#'
#' # Plot map tile
#' g <- plot_tile_station(stn, box, tile_name = "ortofoto")
#'
#' # Add grid and buffer
#' nx <- 200
#' n <- 2
#' g <- add_grid(g, box, nx, n)
#' g <- add_buffer(g, centre, 300, 1000, nx, n)
#' g
#'
#' @importFrom ggplot2 geom_segment geom_label
#'
#' @export
add_grid <- function(g = NULL,
                     box = NULL,
                     nx = NULL,
                     n = 2) {

  # Bind variables to function
  xend <- yend <- NULL

  # Set the interval of the horizontal (y) and vertical lines
  border <- nx * n
  x <- seq(box[1] + border, box[2] - border, by = nx)
  y <- seq(box[3] + border, box[4] - border, by = nx)

  # Set the segment start and end points as data.frame
  dfx <- data.frame(x = x,
                    xend = x,
                    y =    rep(box[3] + border, length(nx)),
                    yend = rep(box[4] - border, length(nx)),
                    row.names = NULL)

  dfy <- data.frame(x =    rep(box[1] + border, length(nx)),
                    xend = rep(box[2] - border, length(nx)),
                    y = y,
                    yend = y,
                    row.names = NULL)

  # Plot the grid as an array of segments
  g <- g +
    geom_segment(data = dfx, aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = .5, color = "gray90") +
    geom_segment(data = dfy, aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = .5, color = "gray90")

  # Add label as scale
  g <- g +
    geom_label(aes(label = sprintf("%i m", nx), x = box[2] - border - nx / 2,
                   y = box[3] + border - nx / 2), size = 2)

  return(g)
}
