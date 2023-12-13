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
#' g <- plot_tile_station(stn, box, tile_name = "esri")
#' g
#' plot_tile_station(stn,box,tile_name="esri", path=path)
#' plot_tile_station(stn,box,tile_name="ar5", path=path)
#' plot_tile_station(stn,box,tile_name="clc", path=path)
#' plot_tile_station(stn,box,tile_name="urban", path=path)
#' plot_tile_station(stn,box,tile_name="osm",dsm=dsm, path="plot/map")
#'
#' @export
plot_tile_station <- function(stn = NULL,
                              box = NULL,
                              tile_name = "osm",
                              dsm = NULL,
                              path = NULL){

  # Libraries
  require(stringr) # str_to_title()
  require(sf)
  require(maptiles) #get_tiles()
  require(tidyterra) # geom_spatraster_rgb() geom_spatraster_contour()
  require(ggplot2)

  # Extract station name and latlon
  stn.name    <- str_to_title(stn$station.name)
  stn.latlon  <- stn %>% st_transform(4326) %>% st_coordinates

  # Reformat name for title in annotate
  stn.title     <- sprintf("station: %s",stn.name)
  stn.subtitle  <- sprintf("id: %i - lat: %02.2f - long: %02.2f - elev:%1.0f",
                           stn$id.stationid, stn.latlon[1], stn.latlon[2], stn$elev)

  # Load tile
  if( tile_name == "osm" ) {
    tile <- get_tiles(box, crop = TRUE, provider="OpenStreetMap")
    credit <- "© OpenStreetMap"

  }else if( tile_name == "esri" ){
    tile <- get_tiles(box, crop = TRUE, provider="Esri.WorldImagery" )
    credit <- "© ESRI WorldImagery"

  }else if( tile_name == "ar5" ){
    tile <- get_tile_wms(box, layer = "ar5")
    credit <- "FKB-AR5 © Nibio"
    legend <- "https://wms.nibio.no/cgi-bin/ar5?version=1.1.1&service=WMS&request=GetLegendGraphic&layer=Arealtype&format=image/png"

    }else if( tile_name == "clc" ){
    tile <- get_tile_wms(box, layer = "CORINE_Land_Cover_2012" )
    credit <- "CORINE LC 2012 © Nibio"
    legend <- "https://wms.nibio.no/cgi-bin/clc?version=1.1.1&service=WMS&request=GetLegendGraphic&layer=CORINE_Land_Cover_2012&format=image/png"

    }else if( tile_name == "urban" ){
    tile <- get_tile_wms(box, layer = "Urban_Atlas_Lu_Lc_2012" )
    credit <- "Urban Atlas 2012 © Nibio"
    legend <- "https://wms.nibio.no/cgi-bin/urban_atlas?version=1.1.1&service=WMS&request=GetLegendGraphic&layer=Urban_Atlas_Lu_Lc_2012&format=image/png"
  }

  # Init ggplot
  g <- ggplot()

  # Plot tile and station location
  g <- g +
    geom_spatraster_rgb(data = tile) +
    geom_sf(data = stn, fill = NA, color='red')

  # Add contour plot from Digital Surface Model
  if(!is.null(dsm)){
    g <- g +
      geom_spatraster_contour(data = dsm, binwidth = 2,alpha=.3)
  }

  # Add coordinate system
  g <- g +
    coord_sf(datum = pull_crs(stn))

  # Set theme and title
  el_hjust <- element_text(hjust = 0.5)
  g <- g +
    theme_minimal() +
    ggtitle(stn.title,subtitle=stn.subtitle) +
    theme(plot.title = el_hjust, plot.subtitle = el_hjust)

  # Set labels for x-axis, y-axis and credits
  g <- g +
    xlab("Easting (metre)") +
    ylab("Northing (metre)") +
    annotate("text", x = Inf, y = -Inf, size = 3,hjust = 1, vjust=0,
             label=sprintf("%s - ETRS89/UTM33" ,credit) )

  # Save plot
  if(!is.null(path)){
    fname <- sprintf("%s/%i_map_%s.png",path,stn$id.stationid,tile_name)
    ggsave(fname,bg="white", width = 7, height = 7)
  }

  return(g)
}
