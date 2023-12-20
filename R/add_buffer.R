#' Add two buffers to a map
#'
#' Add two buffers on a map to assess the area of same land cover type.
#'
#' @param g A ggplot objects on which to add the grid
#' @param box A boundary box object to get the box extent and centre point
#' @param buf1 A distance in metre representing the inner buffer radius
#' @param buf2 A distance in metre representing the outer buffer radius
#' @param nx A number defining the grid interval in x and y
#' @param n A number to set the border of the grid from the edge of the plot
#'
#' @return A ggplot2 object
#'
#' @examples
#' # Get station metadata
#' stn <- get_latlon_frost(stationid=18700)
#'
#' # Get coordinates and define boundary box
#' centre <- sf::st_coordinates(stn)
#' box <- make_bbox(centre,1600)
#'
#' # Plot map tile
#' g <- plot_tile_station(stn, box, tile_name="ortofoto")
#'
#' # Add grid and buffer
#' nx <- 200
#' n <- 2
#' g <- add_grid(g, box, nx, n)
#' g <- add_buffer(g, centre, 300, 1000, nx, n)
#' g
#'
#' @importFrom terra ext vect buffer
#' @importFrom tidyterra pull_crs
#'
#' @export
add_buffer <- function(g,
                       box,
                       buf1,
                       buf2,
                       nx,
                       n){

  # Convert box to SpatExtent and centre to SpatVector
  bbox <- terra::ext(terra::vect(box))
  centre <- cbind(X=mean(bbox[1:2]), Y=mean(bbox[3:4]))
  v <- terra::vect(centre, crs="epsg:25833")

  # Compute segment/label position
  ybuf1 <- bbox[4]-nx*(n-.5)
  ybuf2 <- bbox[4]-nx*(n-1)

  # Add buffers
  g <- g +
    geom_sf(data=terra::buffer(v, buf1), fill=NA, color="black") +
    geom_sf(data=terra::buffer(v, buf2), fill=NA, color="black")

  # Add buffer legend as arrow segments
  g <- g +
    geom_segment(aes(x=centre[1], y=ybuf1, xend=centre[1]-buf1, yend=ybuf1),
                 arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
    geom_segment(aes(x=centre[1], y=ybuf2, xend=centre[1]+buf2, yend=ybuf2),
                 arrow = arrow(length=unit(0.30,"cm"), type = "closed"))

  # Add labels to arrow segments
  # The label for buf1 is placed on the same line than buf2 for clarity
  g <- g +
    geom_label(aes(label=sprintf("%i m",buf1), x=centre[1]-buf1/2, y=ybuf2), size=2) +
    geom_label(aes(label=sprintf("%i m",buf2), x=centre[1]+buf2/2, y=ybuf2), size=2)

  # Fix coordinate system caused by SpatVector conversion
  g <- g + coord_sf(datum = tidyterra::pull_crs(stn))

  return(g)
}
