#' Plot station location with background WMS tiles
#'
#' Plots publicly-available maps, atlas, land cover or satellite imagery near
#' a weather station based on maptiles and custom-made \code{"get_tile_wms"}
#'
#' @references \url{https://github.com/riatelab/maptiles}
#'
#' @param stn A SpatVector with station attributes from \code{"get_latlon_frost"}
#' @param box A SpatExtent defining the area to plot
#' @param tile_name A string defining the type of tile to plot among "osm" (map, default), "esri" (satellite imagery), "ar5" (area type), "clc" (Corine land cover) and "urban" (urban atlas)
#' @param dsm A SpatRaster of a digital surface model around the station, expected radius is 100 m
#' @param path A string path that defines where to save the plot, if NULL (default) the plot is printed on-screen and not saved
#'
#' @return A ggplot object
#'
#' @examples
#' require(sf)
#'
#' # Get station coordinates and name
#' stationid <-  18700
#' stn    <- get_latlon_frost(stationid)
#' centre <- stn  %>% st_coordinates()
#'
#' # Construct box to extract WMS tile
#' dx <- 100
#' box <- c(c(centre[1],centre[2])-dx, c(centre[1],centre[2])+dx) %>% round()
#' class(box) <- "bbox"
#' box <- st_as_sfc(box)
#' st_crs(box) <- 25833 # UTM33
#'
#' # Plot maps using plot_tile_station()
#' g <- plot_tile_station(stn, box, tile_name = "esri")
#' g
#' plot_tile_station(stn, box, tile_name="esri")
#' plot_tile_station(stn, box, tile_name="ar5")
#' plot_tile_station(stn, box, tile_name="clc")
#' plot_tile_station(stn, box, tile_name="urban")
#'
#' # Include Digital Elevation Model as contour
#' dsm   <- download_dem_kartverket(stationid, centre, name="dom", dx, resx=1)
#' plot_tile_station(stn, box, tile_name="osm", dsm=dsm, path="plot/map")
#'
#' @import ggplot2
#' @importFrom sf st_transform st_coordinates
#' @importFrom maptiles get_tiles
#' @importFrom tidyterra geom_spatraster_rgb geom_spatraster_contour pull_crs
#'
#' @export

plot_tile_station <- function(stn = NULL,
                              box = NULL,
                              tile_name = "osm",
                              dsm = NULL,
                              path = NULL){

  # Extract station name and latlon
  stn.name    <- stringr::str_to_title(stn$station.name)
  stn.latlon  <- stn %>% sf::st_transform(4326) %>% sf::st_coordinates()

  # Reformat name for title in annotate
  stn.title     <- sprintf("station: %s",stn.name)
  stn.subtitle  <- sprintf("id: %i - lat: %02.2f - long: %02.2f - elev:%1.0f",
                           stn$id.stationid, stn.latlon[1], stn.latlon[2], stn$elev)

  # Load tile
  if( tile_name == "osm" ) {
    tile <- maptiles::get_tiles(box, crop = TRUE, provider="OpenStreetMap")
    credit <- "\uA9 OpenStreetMap"

  }else if( tile_name == "esri" ){
    tile <- maptiles::get_tiles(box, crop = TRUE, provider="Esri.WorldImagery" )
    credit <- "\uA9 ESRI WorldImagery"

  }else if( tile_name == "ar5" ){
    tile <- get_tile_wms(box, layer = "ar5")
    credit <- "FKB-AR5 \uA9 Nibio"
    legend <- "https://wms.nibio.no/cgi-bin/ar5?version=1.1.1&service=WMS&request=GetLegendGraphic&layer=Arealtype&format=image/png"

    }else if( tile_name == "clc" ){
    tile <- get_tile_wms(box, layer = "CORINE_Land_Cover_2012" )
    credit <- "CORINE LC 2012 \uA9 Nibio"
    legend <- "https://wms.nibio.no/cgi-bin/clc?version=1.1.1&service=WMS&request=GetLegendGraphic&layer=CORINE_Land_Cover_2012&format=image/png"

    }else if( tile_name == "urban" ){
    tile <- get_tile_wms(box, layer = "Urban_Atlas_Lu_Lc_2012" )
    credit <- "Urban Atlas 2012 \uA9 Nibio"
    legend <- "https://wms.nibio.no/cgi-bin/urban_atlas?version=1.1.1&service=WMS&request=GetLegendGraphic&layer=Urban_Atlas_Lu_Lc_2012&format=image/png"
  }

  # Init ggplot
  g <- ggplot()

  # Plot tile and station location
  g <- g +
    tidyterra::geom_spatraster_rgb(data = tile) +
    geom_sf(data = stn, fill = NA, color = 'red')

  # Add contour plot from Digital Surface Model
  if(!is.null(dsm)){
    g <- g +
      tidyterra::geom_spatraster_contour(data = dsm, binwidth = 2, alpha = .3)
  }

  # Add coordinate system
  g <- g +
    coord_sf(datum = tidyterra::pull_crs(stn))

  # Set theme and title
  el_hjust <- element_text(hjust = 0.5)
  g <- g +
    theme_minimal() +
    ggtitle(stn.title, subtitle = stn.subtitle) +
    theme(plot.title = el_hjust, plot.subtitle = el_hjust)

  # Set labels for x-axis, y-axis and credits
  g <- g +
    xlab("Easting (metre)") +
    ylab("Northing (metre)") +
    annotate("text", x = Inf, y = -Inf, size = 3, hjust = 1, vjust=0,
             label = sprintf("%s - ETRS89/UTM33", credit) )

  # Save plot
  if(!is.null(path)){
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    fname <- sprintf("%s/%i_map_%s.png",path, stn$id.stationid, tile_name)
    ggsave(fname, bg="white", width = 7, height = 7)
  }

  return(g)
}
