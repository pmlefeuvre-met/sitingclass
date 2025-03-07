#' Add two buffers to a map
#'
#' Add two buffers on a map to assess the area of same land cover type.
#'
#' @param g A ggplot objects on which to add the buffer
#' @param centre The centre coordinates (lat, lon)
#' @param buf1 A distance in metre representing the inner buffer radius
#' @param buf2 A distance in metre representing the outer buffer radius
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
#' box <- make_bbox(centre, 1600)
#'
#' # Plot map tile
#' g <- plot_tile_station(stn, box, tile_name = "ortofoto")
#'
#' # Add grid and buffer
#' nx <- 200
#' n <- 2
#' g <- add_grid(g, centre, nx, n)
#' g <- add_buffer(g, centre, 300, 1000, nx, n)
#' g
#'
#' @importFrom terra vect buffer
#' @importFrom tidyterra pull_crs
#'
#' @export
add_buffer <- function(g = NULL,
                       centre = NULL,
                       buf1 = NULL,
                       buf2 = NULL,
                       nx = NULL,
                       n = 6) {

  # Create a SpatVector from centre
  v <- terra::vect(centre, crs = "epsg:25833")

  # Compute segment/label position
  ybuf1 <- centre[2] + nx * (n + .5)
  if (!is.null(buf2)) {
    ybuf2 <- centre[2] + nx * (n + 1)
  }

  # Add buffers
  bbuf1 <- terra::buffer(v, buf1)
  g <- g + geom_sf(data = bbuf1, fill = NA, color = "red", linewidth = 2)
  g <- g + geom_sf(data = bbuf1, fill = NA, color = "black", linetype = "longdash", size = 10)
  if (!is.null(buf2)) {
    bbuf2 <- terra::buffer(v, buf2)
    g <- g + geom_sf(data = bbuf2, fill = NA, color = "red", linewidth = 2)
    g <- g + geom_sf(data = bbuf2, fill = NA, color = "black", linetype = "longdash", size = 10)
  }

  # Add buffer legend as arrow segments
  g <- g +
    geom_segment(aes(x = centre[1],
                     y = ybuf1,
                     xend = centre[1] - buf1,
                     yend = ybuf1),
                 arrow = arrow(length = unit(0.30, "cm"), type = "closed"))
  if (!is.null(buf2)) {
    g <- g +
      geom_segment(aes(x = centre[1],
                       y = ybuf2,
                       xend = centre[1] + buf2,
                       yend = ybuf2),
                   arrow = arrow(length = unit(0.30, "cm"), type = "closed"))
  }

  # Add labels to arrow segments
  # The label for buf1 is placed on the same line than buf2 for clarity
  g <- g +
    geom_label(aes(label = sprintf("%i m", buf1),
                   x = centre[1] - buf1 / 2,
                   y = ybuf1),
               size = 2)
  if (!is.null(buf2)) {
    g <- g +
      geom_label(aes(label = sprintf("%i m", buf2),
                     x = centre[1] + buf2 / 2,
                     y = ybuf2),
                 size = 2)
  }

  # Fix coordinate system caused by SpatVector conversion
  g <- g + coord_sf(datum = tidyterra::pull_crs(v))

  return(g)
}
