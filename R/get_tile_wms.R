#' Get Web Mapping Service data tiles
#'
#' Fetch map tiles from publicly available WMS released by Nibio
#'
#' @references \url{https://nibio.no/tjenester/wms-tjenester}
#'
#' @param box A SpatExtent defining the area to plot
#' @param layer A name of a layer provided by WMS such as "ar5", "CORINE_Land_Cover_2012" (default) and "Urban_Atlas_Lu_Lc_2012"
#' @param px A pixel number that defines the resolution of the image/tile, default is 500 px
#'
#' @return A map tile
#'
#' @examples
#' tile <- get_tile_wms(box, layer = "ar5")
#' tile <- get_tile_wms(box, layer = "CORINE_Land_Cover_2012" )
#' tile <- get_tile_wms(box, layer = "Urban_Atlas_Lu_Lc_2012" )
#'
#' @importFrom terra vect ext rast crs
#' @importFrom httr GET content
#' @importFrom magrittr %>%
#'
#' @export

get_tile_wms <- function(box = NULL,
                         layer = "CORINE_Land_Cover_2012",
                         px = 500){

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

  } else if ( layer == "toporaster" ){ #or topografiskraster
    url     <- "http://openwms.statkart.no/skwms1/wms.toporaster4"
    version <- "VERSION=1.3.0"
    crs     <- "CRS=EPSG:25833"
    ref     <- "https://kartkatalog.geonorge.no/metadata/toporaster-4-wms/430b65ec-8543-4387-bf45-dbb5ce4bf4c8"

  } else if ( layer == "ortofoto" ){
    url     <- "https://wms.geonorge.no/skwms1/wms.nib?service=WMS&request=GetCapabilities"
    version <- "VERSION=1.3.0"
    crs     <- "CRS=EPSG:25833"
    ref     <- "https://kartkatalog.geonorge.no/metadata/norge-i-bilder-wms-ortofoto/dcee8bf4-fdf3-4433-a91b-209c7d9b0b0f"
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
  print(wms)
  print(bbox)
  plot(wms)
  plot(bbox)
  ext(wms) <- bbox
  crs(wms) <- "epsg:25833"

  return(wms)
}
