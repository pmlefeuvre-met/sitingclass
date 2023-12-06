# Plot station location with background WMS data
plot_station_tile <- function(stn,box,tile_name="osm",dsm=NULL,path=NULL){
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
    tile <- get_wms_tile(box, layer = "ar5")
    credit <- "FKB-AR5 © Nibio"
    legend <- "https://wms.nibio.no/cgi-bin/ar5?version=1.1.1&service=WMS&request=GetLegendGraphic&layer=Arealtype&format=image/png"
  }else if( tile_name == "clc" ){
    tile <- get_wms_tile(box, layer = "CORINE_Land_Cover_2012" )
    credit <- "CORINE LC 2012 © Nibio"
    legend <- "https://wms.nibio.no/cgi-bin/clc?version=1.1.1&service=WMS&request=GetLegendGraphic&layer=CORINE_Land_Cover_2012&format=image/png"
  }else if( tile_name == "urban" ){
    tile <- get_wms_tile(box, layer = "Urban_Atlas_Lu_Lc_2012" )
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
