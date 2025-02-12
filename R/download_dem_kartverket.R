#' Download digital elevation models from Kartverket's WCS API
#'
#' Build a WCS URL request with `httr2` to download Kartverket's DEM with
#' `terra::rast()` from a bounding box and a DEM type (i.e. "dom" or "dtm").
#' The bounding box is centered to a parsed location and a parsed radius set
#' the extent. The downloaded DEM is a SpatRaster object. If the DEM file
#' already exists, it is loaded by default unless f_overwrite is set to TRUE
#'
#' @references \url{https://kartkatalog.geonorge.no/metadata/nasjonal-hoeydemodell-digital-terrengmodell-25833-wcs/0f0a0f38-00c4-4213-a9e5-2d861dc4abb0}
#' @references \url{https://kartkatalog.geonorge.no/metadata/nasjonal-hoeydemodell-digital-overflatemodell-25833-wcs/e36ea427-13a1-4d7c-be82-977068dfc3e3}
#'
#' @param stn A SpatVector with station attribute `stationid` from
#'        \code{"get_latlon_frost"}
#' @param name A name of the DEM to download, either "dtm" a terrain model
#' @param dx A distance in metre or radius defining the extent of the
#'        bounding box from the centre point
#' @param resx A horizontal resolution in metre
#' @param path A directory path defining where will be saved the data
#' @param f_overwrite A boolean whether the DEM file should be overwritten
#'
#' @return A Digital Elevation Model
#'
#' @examples
#' # Define parameters
#' stn <- get_metadata_frost(stationid = 18700, dx = 100, resx = 1)
#' path   <- "data/dem"
#'
#' # Load data using httr2 and terra
#' dem    <- download_dem_kartverket(stn,name="dtm",path=path)
#' dsm    <- download_dem_kartverket(stn,name="dom",path=path)
#' demkm  <- download_dem_kartverket(stn,name="dtm",dx=20e3,resx=20,path=path)
#'
#' @importFrom httr2 request req_url_query
#' @importFrom terra crds rast writeRaster setMinMax
#'
#' @export

download_dem_kartverket <- function(stn = NULL,
                                    name = "dom",
                                    dx = stn$dx,
                                    resx = stn$resx,
                                    path = "data/dem",
                                    f_overwrite = FALSE) {

  # Extract stationID and centre point of the station
  stationid <- stn$stationid
  centre <- terra::crds(stn)

  # Check if resx matches Kartverket's API requirements
  if (resx < round(dx / 1000)) {
    resx <- ifelse(dx > 1000, round(dx / 1000), 1)
  }

  # Print input parameters
  print(sprintf("Process: %1.0f - %1.1f/%1.1f - %s - %i/%i - path: %s",
                stationid, centre[1], centre[2], name, dx, resx, path))

  # Construct box to extract WMS tile
  box <- make_bbox(stn, dx = dx)

  # Set DEM file name
  fname_out <- sprintf("%s/%1.0f_%s_25833_d%05.0fm_%02.1fm.tif",
                       path,
                       stationid,
                       name,
                       dx,
                       resx)

  # Load demo example for 18700
  if (stationid == 18700) {

    # Set DEM filename and path
    fname <- sprintf("18700_%s_25833_d%05.0fm_%02.1fm.tif", name, dx, resx)
    fpath <- system.file("extdata", fname, package = "sitingclass",
                        mustWork = TRUE)

    # Load DEM as SpatRaster
    dem <- terra::rast(fpath)
    terra::setMinMax(dem)

    # Print file loading and return DEM
    print(sprintf("Load demo file: %s", fname_out))
    return(dem)
  }

  # Verify if Geotiff file exists
  if (file.exists(fname_out) && !f_overwrite) {

    # Load DEM as SpatRaster
    dem <- terra::rast(fname_out)
    terra::setMinMax(dem)

    # Print file loading and return DEM
    print(sprintf("Load existing file: %s", fname_out))
    return(dem)
  }

  # Build URL request with the layer name (i.e. DEM name)
  url  <- sprintf("https://wcs.geonorge.no/skwms1/wcs.hoyde-%s-nhm-25833",
                  name)
  req <- httr2::request(url) |>
    httr2::req_url_query(
      service = "WCS",
      version = "1.0.0",
      request = "GetCoverage",
      format = "GeoTIFF",
      crs = "EPSG:25833",
      coverage = sprintf("nhm_%s_topo_25833", name),
      bbox = paste(box[1],
                   box[3],
                   box[2],
                   box[4], sep = ","),
      RESX = resx,
      RESY = resx)

  # Get the DEM from the URL
  dem <- terra::rast(req$url)

  # Create directory and save files
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  terra::writeRaster(dem, filename = fname_out, overwrite = TRUE)

  # Assign Not-A-Number values and compute MinMax of the DEM
  dem[dem == 0] <- NA
  terra::setMinMax(dem)

  # Return DEM
  return(dem)
}
