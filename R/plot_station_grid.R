#' Plot station with Ortophoto, grid and buffers
#'
#' Overlay a grid and two buffers to count the area of land cover types and
#' assess heat source proximity to a weather station and their distribution.
#' Four different scales are plotted: 10 m, 30 m, 100 m and 1000 m.
#'
#' @param stn A SpatVector with station attributes from \code{"get_latlon_frost"}
#'
#' @return A ggplot2 object
#'
#' @examples
#' # Get station metadata
#' stn    <- get_latlon_frost(stationid=18700)
#'
#' # Plot grid and buffers on four different scales
#' plot_station_grid(stn, path="plot/map")
#'
#' @importFrom sf st_coordinates
#'
#' @export

plot_tile_station <- function(stn = NULL,
                              tile_name="ortofoto",
                              path = NULL){

  # Get coordinates
  centre <- sf::st_coordinates(stn)

  # Directory to save plots
  if(!is.null(path)){
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    fname <- sprintf("%s/%i_map_grid_%s_%04.0f.png",path, stn$id.stationid, tilename)
  }

  # First sale: 1000 metres
  #-----------------------------
  dx <- 1600 # metre
  box <- make_bbox(centre, dx)
  g <- plot_tile_station(stn, box, tile_name="ortofoto")

  # Add grid and buffer
  nx <- 200
  g <- add_grid(g, box, nx)
  g <- add_buffer(g, box, 300, 1000, nx)

  # Save plot
  if(!is.null(path)){
    fname <- sprintf("%s/%i_map_grid_%s_%04.0f.png",path, stn$id.stationid, tilename, dx)
    ggsave(fname, bg="white", width = 7, height = 7)
  }else{g}

  #-----------------------------
  dx <- 160 # metre
  box <- make_bbox(centre, dx)
  g <- plot_tile_station(stn, box, tile_name="ortofoto")

  # Add grid and buffer
  nx <- 20 # metre
  g <- add_grid(g, box, nx)
  g <- add_buffer(g, box, buf1=30, buf2=100, nx)

  # Save plot
  if(!is.null(path)){
    fname <- sprintf("%s/%i_map_grid_%s_%04.0f.png",path, stn$id.stationid, tilename, dx)
    ggsave(fname, bg="white", width = 7, height = 7)
  }else{g}

  #-----------------------------
  dx <- 50 # metre
  box <- make_bbox(centre, dx)
  g <- plot_tile_station(stn, box, tile_name="ortofoto")

  # Add grid and buffer
  nx <- 5 # metre
  g <- add_grid(g, box, nx)
  g <- add_buffer(g, box, buf1=10, buf2=30, nx)

  # Save plot
  if(!is.null(path)){
    fname <- sprintf("%s/%i_map_grid_%s_%04.0f.png",path, stn$id.stationid, tilename, dx)
    ggsave(fname, bg="white", width = 7, height = 7)
  }else{g}

  #-----------------------------
  dx <- 16 # metre
  box <- make_bbox(centre, dx)
  g <- plot_tile_station(stn, box, tile_name="ortofoto")

  # Add grid and buffer
  nx <- 2 # metre
  g <- add_grid(g, box, nx)
  g <- add_buffer(g, box, buf1=3, buf2=10, nx)
  # Save plot
  if(!is.null(path)){
    fname <- sprintf("%s/%i_map_grid_%s_%04.0f.png",path, stn$id.stationid, tilename, dx)
    ggsave(fname, bg="white", width = 7, height = 7)
  }else{g}

}
