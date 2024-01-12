#' Construct boundary box
#'
#' Construct boundary box from sf package
#'
#' @param centre An array of two coordinates in UTM 33 (epsg:25833)
#' @param dx A distance in metre to compute the box boundary as a distance from the centre point
#'
#' @return A bbox object
#'
#' @examples
#' # Get station metadata
#' stn <- get_latlon_frost(stationid=18700)
#'
#' # Get coordinates and define boundary box
#' centre <- sf::st_coordinates(stn)
#' box <- make_bbox(centre,dx=1600)
#'
#' @importFrom sf st_as_sf st_crs
#'
#' @export
make_bbox <- function (centre,
                      dx) {

  # Set box boundary as a distance dx from the centre coordinates
  box <- round(c(c(centre[1], centre[2])-dx,
                  c(centre[1], centre[2])+dx))

  # Assign class bbox
  class(box) <- "bbox"

  # Convert to sfc object (polygon) and set CRS as UTM 33
  box <- sf::st_as_sfc(box)
  sf::st_crs(box) <- 25833

  return(box)
}
