#' Construct boundary box
#'
#' Construct boundary box from sf package
#'
#' @param stn A SpatVector or an array of two coordinates in UTM 33 (epsg:25833)
#' @param dx A distance in metre to compute the box boundary as a distance
#'        from the centre point
#'
#' @return A bbox object
#'
#' @examples
#' # Get station metadata
#' stn <- get_metadata_frost(stationid = 18700)
#'
#' # Define boundary box from SpatVector
#' box <- make_bbox(stn, dx = 1600)
#'
#' # Get coordinates matrix and define boundary box
#' centre <- terra::crds(stn)
#' box <- make_bbox(centre, dx = 1600)
#'
#' @importFrom terra is.valid crds ext
#'
#' @export
make_bbox <- function(stn = NULL,
                      dx = stn$dx) {

  # If object is not a matrix and is a SpatVector, then get its coordinates
  if (!is.matrix(stn)) {
    if (terra::is.valid(stn)) {
      centre <- terra::crds(stn)
    }
  } else {
    centre <- stn
  }

  # Set box boundary as a distance dx from the centre coordinates
  box <- c(floor(c(centre[1], centre[2]) - dx),
           ceiling(c(centre[1], centre[2]) + dx))

  # Assign class SpatExtent
  box <- terra::ext(box[1], box[3], box[2], box[4])

  return(box)
}
