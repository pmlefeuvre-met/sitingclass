#' Weather station context
#'
#' Plots weather station's sun diagram and background maps to assess
#' the exposure of a station and to compute its WMO/met.no siting classification
#'
#' @references \url{https://community.wmo.int/en/activity-areas/imop/siting-classification#MembersTools}
#'
#' @param stationid A station number as integer and defined by met.no
#' @param paramid A parameter number as integer and defined by met.no
#' @param f_verbose A boolean string to print debug messages, default is TRUE
#' @param f_pdf A boolean string to combine all plots into a pdf file
#'
#' @return None
#'
#' @examples
#' # Plot sun diagram and map infos for a weather station
#' #plot_station_siting_context(stationid = 18700)
#' #plot_station_siting_context(stationid = 18700,
#' #                            paramid = 211,
#' #                            f_verbose = TRUE)
#'
#' @importFrom terra crds
#' @importFrom grDevices dev.off pdf
#'
#' @export
plot_station_siting_context <- function(stationid = 18700,
                                        paramid = 211,
                                        f_verbose = FALSE,
                                        f_pdf = FALSE) {

  # Get station coordinates and name
  stn <- get_metadata_frost(stationid,
                            paramid,
                            dx = 100,
                            resx = 1,
                            path = sprintf("output/%1.0f", stationid))

  # Construct box to extract WMS tile
  box <- make_bbox(stn)

  # Print
  if (f_verbose) {
    print(stn$stationid)
    print(stn$station.name)
    print(terra::crds(stn))
    print(box)
  }
  # Plot ESRI imagery
  g0  <- plot_tile_station(stn, box, tile_name = "esri", path = stn$path)
  g10 <- plot_tile_station(stn, box, tile_name = "ar5", path = stn$path)
  g11 <- plot_tile_station(stn, box, tile_name = "clc", path = stn$path)
  g12 <- plot_tile_station(stn, box, tile_name = "urban", path = stn$path)

  # Load data
  f_ow  <- FALSE
  dem   <- download_dem_kartverket(stn,
                                   name = "dtm",
                                   f_overwrite = f_ow)
  dsm   <- download_dem_kartverket(stn,
                                   name = "dom",
                                   f_overwrite = f_ow)

  # Plot OpenStreetMap
  g2 <- plot_tile_station(stn,
                          box,
                          tile_name = "osm",
                          dsm = dsm,
                          path = stn$path)

  # Print
  if (f_verbose) {
    print(dem)
    print(dsm)
  }
  # Plot
  g3 <- plot_station_horizon_sun(stn, path = stn$path)

  # Plot DEM with rayshader
  g4 <- plot_dem_rayshader(stn, dsm, path = stn$path)

  # Save pdf
  if (f_pdf) {
    fname <- sprintf("%s/%1.0f_infos.pdf", stn$path, stationid)
    pdf(fname)
    invisible(lapply(list(g0, g2, g10, g3, g4, g11, g12), print))
    dev.off()
  }
}
