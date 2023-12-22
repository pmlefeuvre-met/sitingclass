#' Compute land cover types
#'
#' Compute land cover types around a station based on retrieved topographic data
#' (buildings, roads, water) and digital terrain/surface models (to compute
#' vegetation height and classify: grass, bush, tree) from Kartverket
#'
#' @references \url{https://kartkatalog.geonorge.no/metadata/fkb-wms/84178e68-f40d-4bb4-b9f6-9bfdee2bcc7a}
#'
#' @param stn A SpatVector with station attributes from \code{"get_latlon_frost"}
#' @param dx A distance in metre or radius defining the extent of the bounding box from the centre point, default `100` metres
#' @param f.plot A boolean whether to plot the land cover types
#'
#' @return A SpatVector of land cover types
#'
#' @examples
#'# Get station metadata
#' stn <- get_latlon_frost(stationid=18700)
#'
#'# Compute land cover
#' compute_landtype(stn, dx=100)
#'
#' @importFrom sf st_coordinates
#' @importFrom terra vect mask
#' @importFrom ggplot2 ggplot scale_fill_manual coord_sf theme_minimal
#' @importFrom tidyterra geom_spatvector
#'
#' @export

compute_landtype <- function(stn=NULL,
                              dx=100,
                              f.plot=FALSE){

# Bind variable to function
  landtype <- NULL

  # Extract stationID and centre point of the station
  stationid <- stn$id.stationid
  centre <- sf::st_coordinates(stn)

  # Construct box to extract WMS tile
  box <- make_bbox(centre, dx)

  # Download DEMs and compute difference to assess vegetation
  dem <- download_dem_kartverket(stationid,centre,name="dtm",dx,resx = 1)
  dsm <- download_dem_kartverket(stationid,centre,name="dom",dx,resx = 1)
  dh  <- dsm - dem

  # Load FKB-AR5 tiles
  px    <- dim(dh)[1]
  building  <- get_tile_wms(box, layer = "bygning", px = px)
  water     <- get_tile_wms(box, layer = "fkb_vann", px = px)
  road      <- get_tile_wms(box, layer = "fkb_samferdsel", px = px)

  # Convert raster tile to vector landcover
  v_building <- raster_to_vector(building,id="building",mask_thr=255)
  v_water    <- raster_to_vector(water   ,id="water"   ,mask_thr=255)
  v_road     <- raster_to_vector(road    ,id="road"    ,mask_thr=255)
  landtype <-  terra::vect(c(v_building,v_water,v_road))

  # Mask already identified land cover
  dh_mask <- terra::mask(dh,landtype,inverse=T)

  # Classify vegetation based on dh thresholds
  v_grass <- raster_to_vector( dh_mask<=.2            ,id="grass",mask_thr = F)
  v_bush  <- raster_to_vector((dh_mask>.2 & dh_mask<3),id="bush" ,mask_thr = F)
  v_tree  <- raster_to_vector( dh_mask>=3             ,id="tree" ,mask_thr = F)

  # Merge all landcover vectors
  landtype <- terra::vect(c(landtype,v_grass,v_bush,v_tree))

  # Convert landcover type values as factors
  levels <-  c("building", "road", "water", "grass", "bush", "tree")
  landtype$landtype <- factor(landtype$value, levels = levels)

  # Plot vector result with fill specific to each factor
  if(f.plot){
    g <- ggplot(data=landtype) +
      tidyterra::geom_spatvector(aes(fill=landtype ),
                                 linewidth=0) +
      scale_fill_manual(values = c("building"="skyblue3",
                                   "road"="azure3",
                                   "water"="cadetblue2",
                                   "grass"="darkolivegreen1",
                                   "bush"="darkolivegreen3",
                                   "tree"="chartreuse4")) +
      coord_sf(datum = tidyterra::pull_crs(box)) +
      theme_minimal()

    print(g)
  }

    # Return merged landcover types as vector
    return(landtype)
}
