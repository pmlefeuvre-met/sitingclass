#' Get station metadata/coordinates from Frost API
#'
#' Fetch stations metadata based on country, parameter id, ... using Frost v1.
#' To handle such a large request output, a pagination protocol is used.
#' The function gets station names, locations and alternate ids.
#'
#' @references \url{https://frost-beta.met.no/docs/codeexamples}
#'
#' @param country A country name as string and defined by met.no
#' @param paramid A parameter number as integer and defined by met.no
#' @param timestamp A time range as timestamp or string "latest"
#' @param hdrshow A JSON string containing metadata structure to extract
#' @param path A path where will be saved the metadata as SpatVector `.gpkg`
#'
#' @return Stations metadata
#'
#' @examples
#' get_metadata_pagination(country = "norge", paramid = 211)
#'
#' @importFrom httr2 request req_url_query req_auth_basic
#' @importFrom httr2 req_headers req_retry req_perform_iterative
#' @importFrom utils type.convert
#' @importFrom tidyr unnest
#' @importFrom terra vect project crds writeVector
#'
#' @export

get_metadata_pagination <- function(
    country = "norge",
    paramid = 211,
    timestamp = "2000-01-01T00:00:00Z/2024-01-01T00:00:00Z",
    hdrshow = '{"id":{},"extra":{"station":{"location":[],"alternateids":[]}}}',
    path = sprintf("output/stn_%s_%i",
                   country,
                   paramid)) {

  # Bind variables to function
  level <- parameterid <- sensor <- stationid  <- NULL
  WIGOS <- lat <-lon <- elev <- NULL

  # Create output directory
  if (!is.null(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }

  # ----------------------------------------------
  # API request with pagination #test_server: "https://v1.frost-dev.k8s.met.no/api/v1/obs/met.no/filter/get"
  url <-  "https://frost-beta.met.no/api/v1/obs/met.no/filter/get"
  resp <- httr2::request(url) |>
    httr2::req_url_query(
      incobs = "false",
      time = timestamp,
      stationcountries = country,
      parameterids = paramid,
      hdrshow = hdrshow,
      itemlimit = "1000"
    ) |>
    httr2::req_headers("X-Frost-Ptsheader" = "first")  |>
    httr2::req_auth_basic(Sys.getenv("FROST_ID"), Sys.getenv("FROST_KEY")) |>
    httr2::req_retry(max_tries = 2) |>
    httr2::req_perform_iterative(next_req, max_reqs = Inf)

  # ----------------------------------------------
  # Format the metadata from JSON response
  meta <- as.data.frame(do.call(rbind,
                                lapply(resp,
                                       function(x) extract_resp(x))))
  meta <- tidyr::unnest(meta,
                        cols = c(level,
                                 parameterid,
                                 sensor,
                                 stationid,
                                 WIGOS,
                                 lat,
                                 lon,
                                 elev))

  # Select and sort stations that in qc_stats for KVALOBS
  meta <- meta[order(meta$stationid), ]

  # ----------------------------------------------
  # Convert station metadata to SpatVector and from Lat-Lon to LCC
  #-- 4326  WGS 84 / Lat Lon
  #-- 32633 WGS 84 / UTM zone 33N
  #-- 25833 ETRS89 / UTM zone 33N
  proj_lcc <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
  stn <- terra::vect(meta,
                     geom = c("lon", "lat"),
                     crs = "epsg:4326")
  stn <- terra::project(stn, proj_lcc)
  # stn <- terra::project(stn, "epsg:25833")

  # Remove Antarctic stations
  coord <- terra::crds(stn)
  stn <- stn[coord[,2] > (-2e6) & coord[,2] < 1.5e6,]

  # Save vector - Can also use GPKG format
  terra::writeVector(stn,
                     filename = sprintf("%s/%s.shp",
                             path,
                             rev(strsplit(path, "/")[[1]])[1]),
                     filetype = "ESRI Shapefile",
                     overwrite = TRUE)

  return(stn)
}

#' Pagination protocol for API request
#'
#' @param resp A request response
#' @param req A request to update
#'
#' @return Updated request headers
#'
#' @importFrom httr2 resp_header req_headers
#'
#' @examples
#' #No example
next_req <- function(resp,
                     req) {
  # Fetch header from response
  header <- httr2::resp_header(resp, "X-Frost-Nextptsheader")

  # Stop pagination if last header is received
  if (header == "last") {
    return(NULL)
  }

  # Update headers
  req <- req |> httr2::req_headers(
    "X-Frost-Ptsheader" = header,
    "X-Frost-Ptsbaseid" = httr2::resp_header(resp, "X-Frost-Nextptsbaseid"),
    "X-Frost-Ptime" = httr2::resp_header(resp, "X-Frost-Nextptsptime"))

  return(req)
}



#' Convert JSON to dataframe
#'
#' @param json A JSON object
#'
#' @return A data.frame
#'
#' @examples
#' #No example
json_to_df <- function(json) {
  df <- as.data.frame(t(as.matrix(unlist(json))))
  return(df)
}


#' Extract and format JSON metadata
#'
#' @param res_1 A JSON object from an individual station or parameterid
#'
#' @return A data.frame
#'
#' @examples
#' #No example
format_resp <- function(res_1){

  ## Extract id infos: level, parameterid, sensor, stationid
  ids  <- json_to_df(res_1$header$id)

  ## Extract alternative ids (WIGOS, ...)
  # Check if alternateids is not empty and contains WIGOS key
  if(!is.null(res_1$header$extra$station$alternateids) &
     any("WIGOS" %in% unlist(res_1$header$extra$station$alternateids))){

    # Convert to dataframe and format: key as name and id as value
    ids_alt <- json_to_df(res_1$header$extra$station$alternateids)
    ids_alt_names <- ids_alt[, names(ids_alt) == "key"]
    ids_alt <- ids_alt[, names(ids_alt) == "id", drop = F]
    colnames(ids_alt) <- ids_alt_names
  } else {
    # Assign empty dataframe if there are no values
    ids_alt <- data.frame(WIGOS="NA")
  }

  ## Extract latest coordinates, hence rev()
  loc <- rev(json_to_df(res_1$header$extra$station$location))
  loc <- loc[c(grep("lat", names(loc))[1],
               grep("lon", names(loc))[1],
               grep("value.elev", names(loc))[1])]
  loc <- as.data.frame(t(apply(loc, 1,as.numeric)))
  names(loc) <- c("lat","lon","elev")

  # Build data.frame with station attributes
  stn_attrib <- cbind(ids,
                      ids_alt['WIGOS'],
                      loc)

  return(stn_attrib)
}



#' Loop through request responses from pagination
#'
#' @param resp A request response
#'
#' @return A matrix
#'
#' @importFrom httr2 resp_body_json
#'
#' @examples
#' #No example
extract_resp <- function(resp) {

  # Convert response to json
  res <- httr2::resp_body_json(resp)

  # Extract the number of elements, i.e. metadata/time series in the response
  n.paramid <- length(res$data$tseries)

  # Loop through each response element
  dat <- t(sapply(1:n.paramid,
                  function(x) format_resp(res$data$tseries[[x]]),
                  simplify = "matrix"))

  return(dat)
}
