#' Compute distance distribution of landcover types
#'
#' Compute distance distribution of land cover types around a station to assess
#' the WMO siting class
#'
#' @references \url{https://community.wmo.int/en/activity-areas/imop/siting-classification}
#'
#' @param stn A SpatVector with station attributes from \code{"get_latlon_frost"}
#' @param landtype A SpatVector with landtype attributes from \code{"compute_landtype"}
#' @param dx A distance in metre or radius defining the extent of the bounding box from the centre point, default `100` metres
#' @param f.plot A boolean whether to plot figures
#'
#' @return A distance distribution per land cover type
#'
#' @examples
#'# Get station metadata
#' stn <- get_latlon_frost(stationid=18700)
#'
#'# Compute land type
#' landtype <- compute_landtype(stn, dx=100, f.plot=TRUE)
#'
#' # Compute land type distance to station
#' landtype_dist <- compute_landtype_distance(stn, landtype, dx=100, f.plot=TRUE)
#'
#' # Compare theoretical Area and from the output (Raster rounding error <5%)
#' plot(rep(as.numeric(rownames(landtype_dist)),2),landtype_dist[,1:2])
#' round((landtype_dist[,1]-landtype_dist[,2])/landtype_dist[,2]*100)
#'
#'
#' @importFrom sf st_coordinates
#' @importFrom terra vect mask
#' @importFrom ggplot2 ggplot scale_fill_manual coord_sf theme_minimal
#' @importFrom tidyterra geom_spatvector
#'
#' @export
compute_landtype_distance <- function(stn=NULL,
                                      landtype=NULL,
                                      dx=200,
                                      f.plot=FALSE){

  # Extract stationID and centre point of the station
  stationid <- stn$id.stationid
  centre <- sf::st_coordinates(stn)

  # Construct box to extract WMS tile
  box <- make_bbox(centre, dx)

  # Download DEMs to set raster reference
  dem <- download_dem_kartverket(stationid,centre,name="dtm",dx,resx = 1)

  # Compute distance from station
  r <- terra::rast(dem)
  dist_stn <- terra::distance(r, stn)

  # Plot station distance in relation to land cover types
  if(f.plot){
    g1 <- ggplot() +
      tidyterra::geom_spatraster(data=dist_stn) +
      geom_sf(data = stn, fill = NA, color = 'red') +
      scale_fill_gradient(low = "grey50", high = "white") +
      tidyterra::geom_spatvector(data=landtype, aes(color = landtype), fill=NA) +
      scale_color_manual(values = c("building"="skyblue3",
                                    "road"="azure3",
                                    "water"="cadetblue2",
                                    "grass"="darkolivegreen1",
                                    "bush"="darkolivegreen3",
                                    "tree"="chartreuse4")) +
      theme_minimal() + coord_sf(datum = tidyterra::pull_crs(r)) +
      theme(legend.position = "bottom")
    print(g1)

    # Plot with ortophoto
    tile <- get_tile_wms(box, layer = "ortofoto")
    # Init ggplot
    g2 <- ggplot() +
      tidyterra::geom_spatraster_rgb(data = tile) +
      geom_sf(data = stn, fill = NA, color = 'red') +
      tidyterra::geom_spatvector(data=landtype, aes(color = landtype), fill=NA) +
      scale_color_manual(values = c("building"="skyblue3",
                                    "road"="azure3",
                                    "water"="cadetblue2",
                                    "grass"="darkolivegreen1",
                                    "bush"="darkolivegreen3",
                                    "tree"="chartreuse4")) +
      theme_minimal() + coord_sf(datum = tidyterra::pull_crs(r)) +
      theme(legend.position = "bottom")
    print(g2)
  }

  # Extract land type factors
  type_array <- levels(landtype$landtype)

  # Assign array with total area to store area/distance distribution per land type
  distance_breaks <- seq(0,dx*1.5,2)
  h <- terra::hist(dist_stn, plot=F, breaks=distance_breaks)
  h_all <- h$counts*prod(terra::res(r)) #array(0,distance_breaks)

  # Loop through land types
  for (type in type_array){
    print(type)

    # Crop distance raster using polygons for a specific land type
    distance <- terra::crop(dist_stn,landtype[landtype$landtype==type,],
                            mask=TRUE, touches=FALSE)

    # Compute histogram
    h <- terra::hist(distance, plot=FALSE, breaks=distance_breaks)

    # Convert count to area in square metre
    h$counts <- h$counts*prod(terra::res(r))

    # Merge distributions
    h_all <- cbind(h_all,h$counts)

    if(f.plot){
      plot(h,
           main=sprintf("landcover: %s",type),
           xlab="Distance in metre",
           ylab="Area in square metre",
           xlim=c(0,150))
    }
  }

  # Compute total area from output and cumulative sums
  h_all <- cbind(rowSums(h_all[,-1]),h_all)
  h_all <- apply(h_all, 2, cumsum)

  # Set column and row names
  colnames(h_all) <- c("tot_area_data","total_area_radius",type_array)
  rownames(h_all) <- distance_breaks[-1]


  return(h_all)
}

