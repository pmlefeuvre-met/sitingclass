#' Weather station context
#'
#' Plots weather station's sun diagram and background maps to assess
#' the exposure of a station and to compute its WMO/met.no siting classification
#'
#' @references \url{https://community.wmo.int/en/activity-areas/imop/siting-classification#MembersTools}
#'
#' @param stationid A station number as integer and defined by met.no
#' @param paramid A parameter number as integer and defined by met.no
#' @param f.verbose.debug A boolean string to print debug messages, default is TRUE
#'
#' @return None
#'
#' @examples
#' plot_station_siting_context(stationid=18700)
#' plot_station_siting_context(stationid=18700,paramid=211,f.debug=T)
plot_station_siting_context <- function(stationid,paramid,f.verbose.debug=TRUE){
  # Get station coordinates and name
  stn <- get_latlon_frost(stationid,paramid)
  centre <- stn  %>% st_coordinates

  # To save files
  path <- sprintf("station_location_files/output/%i",stn$id.stationid)
  dir.create(path,recursive=T,showWarnings=F)

  # Construct box to extract WMS tile
  dx <- 100
  box <- c(c(centre[1],centre[2])-dx,c(centre[1],centre[2])+dx) %>% round
  class(box) <- "bbox"
  box <- st_as_sfc(box)
  st_crs(box) <- 25833 #32633 #to match tile projection

  # Print
  if (f.verbose.debug){
    print(stn$id.stationid)
    print(stn$station.name)
    print(centre)
    print(box)
  }
  # Plot ESRI imagery
  g0  <- plot_station_tile(stn,box,tile_name="esri", path=path)
  g10 <- plot_station_tile(stn,box,tile_name="ar5", path=path)
  g11 <- plot_station_tile(stn,box,tile_name="clc", path=path)
  g12 <- plot_station_tile(stn,box,tile_name="urban", path=path)

  # Load data
  f.ow <- FALSE
  dem   <- download_dem_kartverket(stationid,centre,name="dtm",dx,resx,f.overwrite=f.ow)
  dsm   <- download_dem_kartverket(stationid,centre,name="dom",dx,resx,f.overwrite=f.ow)
  demkm <- download_dem_kartverket(stationid,centre,name="dtm",20e3,20,f.overwrite=f.ow)
  #ar5 <- load_data_ar5(box,f.wms=F)

  # Plot OpenStreetMap
  g2 <- plot_station_tile(stn,box,tile_name="osm",dsm=dsm, path=path)

  # Print
  if (f.verbose.debug){
    print(dem)
    print(dsm)
  }
  # Plot
  g3 <- plot_station_horizon_sun(stn,dem,dsm,demkm, path=path)

  # Plot DEM with rayshader
  g4 <- plot_dem_rayshader(stn,dsm, path=path)

  # Save pdf
  fname <- sprintf("%s/%i_infos.pdf",path,stn$id.stationid)
  pdf(fname)
  invisible(lapply(list(g0,g2,g10,g3,g4,g11,g12), print))
  dev.off()
}
