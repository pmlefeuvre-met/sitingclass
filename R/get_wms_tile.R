# Load WMS data
get_wms_tile <- function(box, layer = "CORINE_Land_Cover_2012", px = 500){
  # Libraries
  require(httr)
  require(terra)

  # Extract bounding box
  bbox <- ext(vect(box))

  # Set URL options to get data
  if( layer == "ar5"){
    layer   <- "Arealtype"
    url     <- "https://wms.nibio.no/cgi-bin/ar5"
    version <- "VERSION=1.1.1"
    crs     <- "SRS=EPSG:25833"
  } else if ( layer == "CORINE_Land_Cover_2012"){
    url     <- "https://wms.nibio.no/cgi-bin/clc"
    version <- "VERSION=1.1.1"
    crs     <- "SRS=EPSG:25833"
  } else if ( layer == "Urban_Atlas_Lu_Lc_2012"){
    url     <- "https://wms.nibio.no/cgi-bin/urban_atlas"
    version <- "VERSION=1.3.0"
    crs     <- "CRS=EPSG:25833"
  }

  # Set WMS connection
  con <- paste(url,
               paste(
                 "SERVICE=WMS",
                 version,
                 "request=GetMap",
                 "FORMAT=image/png",
                 crs,
                 sprintf("LAYERS=%s", layer),
                 sprintf("bbox=%1.0f,%1.0f,%1.0f,%1.0f",bbox[1],bbox[3],bbox[2],bbox[4]),
                 sprintf("WIDTH=%i", px),
                 sprintf("HEIGHT=%i", px),
                 sep = "&"),
               sep = "?")

  # Load WMS and convert to SpatRaster
  wms <- GET(con) %>% content %>% "*"(255) %>% rast
  names(wms) <- c("red", "green", "blue")
  ext(wms) <- ext(vect(box))
  crs(wms) <- "epsg:25833"

  return(wms)
}
