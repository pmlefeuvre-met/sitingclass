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
#' landtype <- compute_landtype(stn, dx=100)
#'
#' # Compute land type distance to station
#' compute_landtype_distance(stn, landtype)
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
                                      f.pdf=FALSE){

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
  dist_stn03  <- terra::mask(dist_stn,(dist_stn<=3  ),maskvalues=F)
  dist_stn05  <- terra::mask(dist_stn,(dist_stn<=5  ),maskvalues=F)
  dist_stn10  <- terra::mask(dist_stn,(dist_stn<=10 ),maskvalues=F)
  dist_stn30  <- terra::mask(dist_stn,(dist_stn<=30 ),maskvalues=F)
  dist_stn100 <- terra::mask(dist_stn,(dist_stn<=100),maskvalues=F)

  # Plot station distance in relation to land cover types
  if(f.plot){
    ggplot()+
      tidyterra::geom_spatraster(data=dist_stn) +
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
  }

  # Assign empty array to store area/distance distribution
  h_all=array(0,length(h$counts))

  # Extract land types
  type_array <- levels(landtype$landtype)

  # Loop through land types
  for (type in type_array){
    print(type)

    # Crop distance raster using polygons for a specific land type
    distance <- terra::crop(dist_stn,landtype[landtype$landtype==type,],mask=T)

    # Compute histogram
    h <- terra::hist(distance, plot=F, breaks=seq(0,dx*1.5,2))

    # Convert count to area
    h$counts <- h$counts*prod(terra::res(r)) # from count to area in square meter

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

  # Clean, remove empty first column and set column names
  h_all <- h_all[,-1]
  colnames(h_all) <- type_array

  return(h_all)
}
