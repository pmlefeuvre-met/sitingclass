#' Convert SpatRaster to SpatVector
#'
#' Convert a raster to polygon, remove raster background from vector (white for image, FALSE for mask),
#' aggregate single polygons and set vector values as a single id.
#'
#' @references \url{https://rspatial.r-universe.dev/terra/doc/manual.html#as.polygons}
#'
#' @param raster A SpatRaster image or mask
#' @param id A string or number defining a common factor for all generated polygons
#' @param mask_thr A number or boolean defining a background value to remove
#' @param f.plot A boolean whether to plot the vectorisation result
#'
#' @return A SpatVector
#'
#' @examples
#' # Load station
#' stn <- get_latlon_frost(stationid=18700)
#' centre <- sf::st_coordinates(stn)
#'
#' # Construct box with 200 m radius to extract WMS tile
#' box <- make_bbox(centre, dx=200)
#'
#' # Load a tile
#' building  <- get_tile_wms(box, layer = "bygning", px = px)
#'
#' # Convert raster tile to vector landcover
#' v_building <- raster_to_vector(building,id="building",mask_thr=255)
#'
#' @importFrom terra as.polygons aggregate setValues
#' @importFrom ggplot2 ggplot
#' @importFrom tidyterra geom_spatvector
#'
#' @export
#'
#
raster_to_vector <- function(raster,
                             id="undefined",
                             mask_thr=255,
                             f.plot=FALSE){

  # Convert raster to vector (i.e. polygons)
  vector <- terra::as.polygons(raster)

  # Remove background: white background (255) or mask (FALSE)
  vector <- vector[!(vector[[1]]==mask_thr)]

  # Aggregates single vectors
  vector <- terra::aggregate(vector, by=names(vector))

  # Set vector ids
  vector <- terra::setValues(vector,id)

  # Plot
  if(f.plot){
    print( ggplot(data=vector) + tidyterra::geom_spatvector(aes(fill=id), linewidth=0) )
  }

  return(vector)
}
