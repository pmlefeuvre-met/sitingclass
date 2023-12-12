#' Download digital elevation models from Kartverket's WCS API
#'
#' Define a GetCapabilities request URL using OSW4R and Kartverket's Web Coverage Service
#' that downloads a DEM from a bounding box and a DEM type (i.e. "dom" or "dtm").
#' The bounding box is centered to a parsed location and a parsed radius set the
#' extent. The downloaded DEM is a SpatRaster object. If the DEM file already
#' exists, it is loaded by default unless f.overwrite is set to TRUE
#'
#' @references \url{https://kartkatalog.geonorge.no/metadata/nasjonal-hoeydemodell-digital-terrengmodell-25833-wcs/0f0a0f38-00c4-4213-a9e5-2d861dc4abb0}
#' @references \url{https://kartkatalog.geonorge.no/metadata/nasjonal-hoeydemodell-digital-overflatemodell-25833-wcs/e36ea427-13a1-4d7c-be82-977068dfc3e3}
#' @references \url{https://cran.r-project.org/web/packages/ows4R/vignettes/wcs.html}
#'
#' @param stationid A station ID used for the DEM file name
#' @param centre A coordinate array (i.e. c(x, y)) of the station in UTM 33 (epsg:25833)
#' @param name A name of the DEM to download, either "dtm" a terrain model or the default "dom" a surface model
#' @param dx A distance in metre or radius defining the extent of the bounding box from the centre point, default 100 metres
#' @param resx A horizontal resolution in metre, default is dx/100 if greater than 1 metre
#' @param f.overwrite A boolean whether the DEM file should be overwritten, default FALSE
#'
#' @return A Digital Elevation Model
#'
#' @examples
#' # Load data
#' stationid <- 18700
#' centre <- stn  %>% st_coordinates
#' path   <- "station_location_files/dem"
#' dem    <- download_dem_kartverket(stationid,centre,name="dtm",dx=100,resx=1,path=path)
#' dsm    <- download_dem_kartverket(stationid,centre,name="dom",dx=100,resx=1,path=path)
#' demkm  <- download_dem_kartverket(stationid,centre,name="dtm",dx=20e3,resx=20,path=path)
#'
#' @export
download_dem_kartverket <- function(stationid = NULL,
                                    centre = NULL,
                                    name = "dom",
                                    dx = 100,
                                    resx = 1,
                                    path = "dem",
                                    f.overwrite = FALSE){

  # Libraries
  require(ows4R) # WCSClient$new() getCapabilities()
  require(terra) # rast() setMinMax()

  # Print input parameters
  print(sprintf("Process: %i - %1.1f/%1.1f - %s - %i/%i - path: %s",
                stationid, centre[1], centre[2], name, dx, resx, path))

  # Compute bounding box
  box <- c(c(centre[1],centre[2])-dx,c(centre[1],centre[2])+dx) %>% round

  # Set horizontal resolution if not defined
  if(is.null(resx)){resx <- dx/100}

  # Set DEM file name
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  fname_out <- sprintf("%s/%i_%s_25833_d%05.0fm_%im.tif",path,stationid,name,dx,resx)

  # Verify if file exists
  if(file.exists(fname_out) && !f.overwrite){

    # Load DEM as SpatRaster
    dem <- terra::rast(fname_out)
    terra::setMinMax(dem)

    # Print file loading and return DEM
    print(sprintf("Load existing file: %s",fname_out))
    return(dem)

    }

  # Get WCS info from an URL request with the layer name (i.e. DEM name)
  url  <- sprintf("https://wcs.geonorge.no/skwms1/wcs.hoyde-%s-nhm-25833",name)
  WCS  <- WCSClient$new(url,serviceVersion = "1.0.0", logger = "INFO")
  caps <- WCS$getCapabilities()
  chla <- caps$findCoverageSummaryById(sprintf("nhm_%s_topo_25833",name), exact = T)

  # Send URL request to download the DEM data.
  dem <- chla$getCoverage(crs = "EPSG:25833",RESX = resx, RESY = resx,
                          bbox=OWSUtils$toBBOX(box[1],box[3],box[2],box[4]),
                          filename = fname_out)

  # Assign Not-A-Number values and compute MinMax of the DEM
  dem[dem==0] <- NA
  terra::setMinMax(dem)

  return(dem)
}
