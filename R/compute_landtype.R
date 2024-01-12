#' Compute land cover types
#'
#' Compute land cover types around a station based on retrieved topographic data
#' (buildings, roads, water) and digital terrain/surface models (to compute
#' vegetation height and classify: grass, bush, tree) from Kartverket. The
#' horizontal resolution of the digital elevation models defines the resolution
#' of the tile of the topographic data.
#'
#' @references \url{https://kartkatalog.geonorge.no/metadata/fkb-wms/84178e68-f40d-4bb4-b9f6-9bfdee2bcc7a}
#'
#' @param stn A SpatVector with station attributes from
#'        \code{"get_latlon_frost"}
#' @param dx A distance in metre or radius defining the extent of the
#'        bounding box from the centre point
#' @param resx A horizontal resolution in metre
#' @param f_plot A boolean whether to plot the land cover types
#'
#' @return A SpatVector of land cover types
#'
#' @examples
#'# Get station metadata
#' stn <- get_latlon_frost(stationid=18700)
#'
#'# Compute land cover
#' compute_landtype(stn, dx=100, resx=1, f_plot=TRUE)
#'
#' @importFrom sf st_coordinates
#' @importFrom terra vect mask erase
#' @importFrom ggplot2 ggplot scale_fill_manual coord_sf theme_minimal
#' @importFrom tidyterra geom_spatvector
#'
#' @export

compute_landtype <- function(stn = NULL,
                             dx = 100,
                             resx = 1,
                             f_plot = FALSE) {

  # Bind variable to function
  landtype <- NULL

  # Extract centre point of the station
  centre <- sf::st_coordinates(stn)

  # Construct box to extract WMS tile
  box <- make_bbox(centre, dx)

  # Download DEMs and compute difference to assess vegetation
  dem <- download_dem_kartverket(stn, name = "dtm", dx, resx)
  dsm <- download_dem_kartverket(stn, name = "dom", dx, resx)
  dh  <- dsm - dem

  # Load FKB-AR5 tiles
  px    <- dim(dh)[1]
  building  <- get_tile_wms(box, layer = "bygning", px = px)
  road      <- get_tile_wms(box, layer = "fkb_samferdsel", px = px)
  water     <- get_tile_wms(box, layer = "fkb_vann", px = px)
  print("Loaded WMS tiles")

  # Convert raster tile to vector landcover
  v_building <- raster_to_vector(building,
                                 id = "building",
                                 mask_thr = 255)
  v_road     <- raster_to_vector(road,
                                 id = "road",
                                 mask_thr = 255)
  v_water    <- raster_to_vector(water,
                                 id = "water",
                                 mask_thr = 255)
  landtype <-  terra::vect(c(v_building, v_road, v_water))
  print("Vectorised WMS tiles")

  # Mask already identified land cover
  dh_mask <- terra::mask(dh, landtype, inverse = TRUE, touches = FALSE)

  # Classify vegetation based on dh thresholds in metre
  v_grass <- raster_to_vector(dh_mask <= .10,
                              id = "grass",
                              mask_thr = FALSE)
  v_crop  <- raster_to_vector((dh_mask > .10 & dh_mask <= .25),
                              id = "crop",
                              mask_thr = FALSE)
  v_bush  <- raster_to_vector((dh_mask > .25 & dh_mask <= 3),
                              id = "bush",
                              mask_thr = FALSE)
  v_tree  <- raster_to_vector(dh_mask >= 3,
                              id = "tree",
                              mask_thr = FALSE)
  print("Vectorised vegetation")

  # Merge all landcover vectors
  landtype <- terra::vect(c(landtype, v_grass, v_crop, v_bush, v_tree))

  # Convert landcover type values to factors
  levels <-  c("building", "road", "water", "grass", "crop", "bush", "tree")
  landtype$landtype <- factor(landtype$value, levels = levels)
  landtype <- landtype[, 2]

  # Erase overlapping vectors with hierarchy defined by the order of levels
  landtype <- terra::erase(landtype[order(landtype$landtype,
                                          decreasing = TRUE), ],
                           sequential = TRUE)
  print("Erased overlapping vectors")

  # Plot vector result with fill specific to each factor
  if (f_plot) {
    g <- ggplot(data = landtype) +
      tidyterra::geom_spatvector(aes(fill = landtype),
                                 linewidth = 0) +
      scale_fill_manual(values = fill_landtype) +
      coord_sf(datum = tidyterra::pull_crs(box)) +
      theme_minimal() +
      theme(legend.position = "bottom")

    print(g)
  }

  # Return merged landcover types as vector
  return(landtype)
}
