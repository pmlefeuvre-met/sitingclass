#' Add grid/buffer
#'
#' Add a grid and add buffers on a map to assess the area of same land cover
#' type.
#'
#' @param g A ggplot objects on which to add the grid
#' @param box A boundary box object to get the box extent
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
#' @importFrom terra ext vect
#'
#' @export
add_grid <- function(g = NULL,
                     box = NULL,
                     nx = NULL,
                     n = 2) {

  # Bind variables to function
  xend <- yend <- NULL

  # Define box boundary
  bbox <- terra::ext(terra::vect(box))

  # Set the interval of the horizontal (y) and vertical lines
  border <- nx * n
  x <- seq(bbox[1] + border, bbox[2] - border, by = nx)
  y <- seq(bbox[3] + border, bbox[4] - border, by = nx)

  # Set the segment start and end points as data.frame
  dfx <- data.frame(x = x,
                    xend = x,
                    y =    rep(bbox[3] + border, length(nx)),
                    yend = rep(bbox[4] - border, length(nx)),
                    row.names = NULL)

  dfy <- data.frame(x =    rep(bbox[1] + border, length(nx)),
                    xend = rep(bbox[2] - border, length(nx)),
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
    geom_label(aes(label = sprintf("%i m", nx), x = bbox[2] - border - nx / 2,
                   y = bbox[3] + border - nx / 2), size = 2)

  return(g)
}
