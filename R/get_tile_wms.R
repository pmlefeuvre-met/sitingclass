#' Get Web Mapping Service data tiles
#'
#' Fetch map tiles from publicly available WMS released by Nibio
#'
#' @references \url{https://nibio.no/tjenester/wms-tjenester}
#'
#' @param box A SpatExtent defining the area to plot
#' @param layer A name of a layer provided by WMS such as "ar5", "toporaster",
#' "CORINE_Land_Cover_2012", "Urban_Atlas_Lu_Lc_2012","ortofoto",
#' "fkb_arealdekke", "fkb_arealdekke","fkb_vann","vann_omrade","fkb_samferdsel",
#' "veg","fkb_bygning","bygning","fkb_naturinfo", "naturinfo"
#' @param px A pixel number that defines the resolution of the image/tile, default is 500 px
#'
#' @return A map tile
#'
#' @examples
#' require(sf)
#'
#' # Get station coordinates and name
#' stn    <- get_latlon_frost(18700)
#' centre <- sf::st_coordinates(stn)
#'
#' # Construct box to extract WMS tile
#' dx <- 100
#' box <- c(c(centre[1],centre[2])-dx, c(centre[1],centre[2])+dx) %>% round()
#' class(box) <- "bbox"
#' box <- st_as_sfc(box)
#' st_crs(box) <- 25833 # UTM33
#'
#' # Load tiles
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
                         layer = "ar5",
                         px = 500){

  # Extract bounding box
  bbox <- terra::ext(terra::vect(box))

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
    url     <- "https://wms.geonorge.no/skwms1/wms.nib"
    version <- "VERSION=1.3.0"
    crs     <- "CRS=EPSG:25833"
    ref     <- "https://kartkatalog.geonorge.no/metadata/norge-i-bilder-wms-ortofoto/dcee8bf4-fdf3-4433-a91b-209c7d9b0b0f"
  } else if ( any(layer %in% c("ar5","fkb_arealdekke","fkb_vann","vann_omrade",
                               "fkb_samferdsel","veg","fkb_bygning","bygning",
                               "fkb_naturinfo","naturinfo")) ){
    url     <- "https://openwms.statkart.no/skwms1/wms.fkb"
    version <- "VERSION=1.3.0"
    crs     <- "CRS=EPSG:25833"
    ref     <- "https://kartkatalog.geonorge.no/metadata/fkb-wms/84178e68-f40d-4bb4-b9f6-9bfdee2bcc7a"
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
  wms <- httr::GET(con) %>% httr::content() %>% "*"(255) %>% terra::rast()
  if (dim(wms)[3] == 3) {
    names(wms) <- c("red", "green", "blue")
  }else if(dim(wms)[3] == 4){
    names(wms) <- c("red", "green", "blue", "alpha")
  }
  terra::ext(wms) <- bbox
  terra::crs(wms) <- "epsg:25833"

  return(wms)
}
