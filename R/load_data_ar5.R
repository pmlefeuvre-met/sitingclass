#' Load FKB-AR5 from vector files stored locally (restricted access to download)
#'
#' Load vector FKB-AR5 files to assess area type aka land cover, alternatively
#' get WMS tile (deprecated, use \code{"get_tile_wms"})
#'
#' @references \url{https://kartkatalog.geonorge.no/metadata/fkb-ar5/166382b4-82d6-4ea9-a68e-6fd0c87bf788}
#'
#' @param box A SpatExtent defining the area to plot
#' @param f_wms A boolean to switch to WMS image tile instead of loading file, default TRUE (deprecated)
#' @param layer A name of a layer provided by WMS. For "ar5", it is "Arealtype" as default
#' @param px A pixel number that defines the resolution of the image/tile, default is 500 px
#'
#' @return A map tile
#'
# #' @examples
# #' ar5 <- load_data_ar5(box,f_wms=FALSE)
#'
# #' @import sf
# #' @import httr
# #' @import terra

load_data_ar5 <- function(box = NULL,
                          f_wms = TRUE,
                          layer = "Arealtype",
                          px = 500){

  # Convert to UTM 33 and clip vector to box
  if(!f_wms){

    # Path and filename
    path <- "station_location_files/fkb-ar5"
    fname <- "Basisdata_03_Oslo_25832_FKB-AR5_FGDB.gdb"
    # fname <- "Basisdata_0000_Norge_25833_FKB-AR5_FGDB.gdb"

    # Layer's info
    #st_layers(sprintf("%s/%s",path,fname))

    # Load file
    ar5 <- sf::st_read(sprintf("%s/%s",path,fname),layer="fkb_ar5_omrade")
    ar5 <- ar5["arealtype"] %>% sf::st_transform(25833) %>% sf::st_intersection(box)
    ar5 <- terra::vect(ar5) #SpatVector

  }else{
    bbox <- terra::ext(terra::vect(box))
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
    ar5 <- httr::GET(con) %>% httr::content %>% "*"(255) %>% terra::rast
    names(ar5) <- c("red", "green", "blue")
    terra::ext(ar5) <- bbox
    terra::crs(ar5) <- "epsg:25833"
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
