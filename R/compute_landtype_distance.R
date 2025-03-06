#' Compute distance distribution of landcover types
#'
#' Compute distance distribution of land cover types around a station to assess
#' the WMO siting class
#'
#' @references \url{https://community.wmo.int/en/activity-areas/imop/siting-classification}
#'
#' @param stn A SpatVector with station attributes from
#'        \code{"get_latlon_frost"}
#' @param landtype A SpatVector with landtype attributes from
#'        \code{"compute_landtype"}
#' @param f_plot A boolean whether to plot figures
#'
#' @return A distance distribution per land cover type
#'
#' @examples
#'# Get station metadata
#' stn <- get_metadata_frost(stationid = 18700, dx = 100, resx = 1, path = NULL)
#'
#'# Compute land type
#' landtype <- compute_landtype(stn, f_plot = TRUE)
#'
#' # Compute land type distance to station
#' landtype_dist <- compute_landtype_distance(stn, landtype, f_plot = TRUE)
#'
#' @importFrom terra vect mask rast
#' @importFrom ggplot2 ggplot scale_fill_manual coord_sf theme_minimal
#' @importFrom tidyterra geom_spatvector
#'
#' @export
compute_landtype_distance <- function(stn = NULL,
                                      landtype = NULL,
                                      f_plot = FALSE) {

  # Construct box to extract WMS tile
  box <- make_bbox(stn)

  # Download DEMs to set raster reference
  dem <- download_dem_kartverket(stn, name = "dtm")

  # Compute distance from station
  r <- terra::rast(dem)
  dist_stn <- terra::distance(r, stn)

  if (f_plot) {
    # Plot ortophoto and classification (limit to areas < 500m)
    if (stn$dx < 500) {
      # Load demo tile for 18700 or Get WMS tile
      if ((stn$stationid == 18700) & (stn$dx == 100)){
        fpath <- system.file("extdata", "18700_ortophoto_25833_d00100m.tif",
                             package = "sitingclass", mustWork = TRUE)
        tile <- terra::rast(fpath)
      }else{
        # Get WMS tile and plot
        tile <- get_tile_wms(box, layer = "ortofoto")
      }

      # Plot
      g <- ggplot() +
        tidyterra::geom_spatraster_rgb(data = tile) +
        geom_sf(data = stn, fill = NA, color = "red") +
        tidyterra::geom_spatvector(data = landtype,
                                   aes(color = landtype),
                                   fill = NA) +
        scale_color_manual(values = fill_landtype) +
        theme_minimal() + coord_sf(datum = tidyterra::pull_crs(r)) +
        theme(legend.position = "bottom")

      if (is.null(stn$path)) {
        print(g)
      } else {
        fname <- sprintf("%s/%1.0f_landtype_map_orto_%04.0fm.png",
                         stn$path,
                         stn$stationid,
                         stn$dx)
        ggsave(fname, bg = "white", width = 7, height = 7)
      }
    }
  }

  # Extract land type factors
  type_array <- levels(landtype$landtype)

  # Make histogram breaks every 1 m first and then 2 m after 30m
  distance_breaks <- c(0:29, seq(30, stn$dx * 1.5, 2))

  # Compute histogram from distance raster for theoretical total area
  h <- terra::hist(dist_stn, plot = FALSE, breaks = distance_breaks)

  # Convert to area and assign array to store area distribution per land type
  h_all <- h$counts * prod(terra::res(r))
  l_h <- length(h$counts)

  # Loop through land types
  for (type in type_array) {
    #print(type)
    landtype_select <- landtype[landtype$landtype == type, ]

    # Check if vector is empty
    if (terra::is.empty(landtype_select)) {
      # Merge with rest of distribution data
      h_all <- cbind(h_all, rep(0,l_h))
      next
    }

    # Crop distance raster using polygons for a specific land type
    distance <- terra::crop(dist_stn, landtype_select,
                            mask = TRUE, touches = FALSE)

    # Compute histogram
    h <- terra::hist(distance, plot = FALSE, breaks = distance_breaks)

    # Convert count to area in square metre
    h$counts <- h$counts * prod(terra::res(r))

    # Merge with rest of distribution data
    h_all <- cbind(h_all, h$counts)
  }

  # Compute total area from output and cumulative sums
  h_all <- apply(h_all, 2, cumsum)

  # Set column and row names
  colnames(h_all) <- c("total_area", type_array)
  rownames(h_all) <- distance_breaks[-1]

  # Keep only data inside the radius dx and avoid corner effect from bbox
  h_all <- h_all[1:which(distance_breaks == stn$dx), ]

  return(h_all)
}
