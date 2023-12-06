# Load FKB-AR5 (stored locally)
load_data_ar5 <- function(box, f.wms = TRUE, layer = "Arealtype", px = 500){
  # Libraries
  require(sf)
  require(httr)
  require(terra)

  # Convert to UTM 33 and clip vector to box
  if(!f.wms){
    # Path and filename
    path <- "station_location_files/fkb-ar5"
    fname <- "Basisdata_03_Oslo_25832_FKB-AR5_FGDB.gdb"
    # fname <- "Basisdata_0000_Norge_25833_FKB-AR5_FGDB.gdb"

    # Layer's info
    #st_layers(sprintf("%s/%s",path,fname))

    # Load file
    ar5 <- sf::st_read(sprintf("%s/%s",path,fname),layer="fkb_ar5_omrade")
    ar5 <- ar5["arealtype"] %>% st_transform(25833) %>% st_intersection(box)
    ar5 <- vect(ar5) #SpatVector

  }else{
    bbox <- ext(vect(box))
    con <- paste("https://wms.nibio.no/cgi-bin/ar5",
                 paste(
                   "SERVICE=WMS",
                   "VERSION=1.1.1",
                   "request=GetMap",
                   "FORMAT=image/png",
                   "SRS=EPSG:25833",
                   sprintf("LAYERS=%s", layer),
                   sprintf("bbox=%1.0f,%1.0f,%1.0f,%1.0f",bbox[1],bbox[3],bbox[2],bbox[4]),
                   sprintf("WIDTH=%i", px),
                   sprintf("HEIGHT=%i", px),
                   sep = "&"),
                 sep = "?")
    ar5 <- GET(con) %>% content %>% "*"(255) %>% rast
    names(ar5) <- c("red", "green", "blue")
    ext(ar5) <- ext(vect(box))
    crs(ar5) <- "epsg:25833"
  }
  # https://wms.nibio.no/cgi-bin/ar5?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&FORMAT=image/png&WIDTH=500&HEIGHT=500&LAYERS=Arealtype&SRS=EPSG:25833&BBOX=261750,6606750,262000,6607000
  # http://mesonet.agron.iastate.edu/cgi-bin/mapserv/mapserv?map=/mesonet/www/apps/iemwebsite/data/wms/goes/conus_ir.map&SERVICE=WMS&REQUEST=GetMap&VERSION=1.3.0&WIDTH=256&HEIGHT=256&FORMAT=image/png&TRANSPARENT=TRUE&BBOX=24,-126,50,-66&LAYERS=conus_ir_4km_900913,conus_ir_4km&CRS=EPSG:4326&STYLES&

  # PLOT
  # ggplot()+
  # geom_spatvector(data=ar5,aes(fill = arealtype), fill=NA) +
  # coord_sf(datum = pull_crs(ar5)) +
  # theme_minimal() +
  # theme(legend.position = "bottom")
  return(ar5)
}
