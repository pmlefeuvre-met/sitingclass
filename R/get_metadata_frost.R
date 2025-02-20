#' Get station and sensor metadata from Frost API
#'
#' Fetch station metadata based on station number & parameter id from Frost v1.
#' The function gets station name, location and sensor details such as level,
#' exposure and performance
#'
#' @references \url{https://frost-beta.met.no/docs/codeexamples}
#'
#' @param stationid A station number as integer and defined by met.no
#' @param paramid A parameter number as integer and defined by met.no
#' @param dx A distance in metre or radius defining the extent of the
#'        bounding box from the centre point
#' @param resx A horizontal resolution in metre
#' @param path A directory path defining where will be saved project plots/data
#'
#' @return Station metadata
#'
#' @examples
#' get_metadata_frost(stationid = 18700)
#' get_metadata_frost(stationid = 18700, paramid = 211)
#' get_metadata_frost(stationid = 18700, paramid = 211, dx = 100, resx = 1)
#'
#' @importFrom httr2 request req_retry req_auth_basic
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom utils type.convert
#' @importFrom terra vect project
#'
#' @export

get_metadata_frost <- function(stationid = 18700,
                               paramid = NULL,
                               dx = 100,
                               resx = 1,
                               path = sprintf("output/%i", stationid)) {

  # Load demo data
  if ((stationid == 18700) & is.null(paramid) & (dx == 100)) {
    ## Get demo data files
    fpath <- system.file("extdata", "18700_stn.gpkg",
                         package = "sitingclass", mustWork = TRUE)
    # Load demo data
    stn  <- terra::vect(fpath)
    return(stn)
  }

  # Create output directory
  if (!is.null(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }

  # Define Frost URL
  url <- "https://frost-beta.met.no/api/v1/obs/met.no/filter/get?"

  # Build request with parameters
  req <- httr2::request(url)  |>
    httr2::req_url_query(
      incobs = "false",
      stationids = stationid,
      time = "1000-01-01T00:00:00Z/2100-01-01T00:00:00Z", #latest
      basicoutput = "false")
  if (!is.null(paramid)) {
    req <- req |> httr2::req_url_query(parameterids=paramid)
  }

  # Add retry query and authentication
  req <- req |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE) |>
    httr2::req_auth_basic(Sys.getenv("FROST_ID"), Sys.getenv("FROST_KEY"))

  # Get response as json
  res <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  res_1 <- res$data$tseries[[1]]

  # Extract ids, names and organisation
  ids  <- json_to_df(res_1$header$id)

  # Extract alternative ids (WMO,...)
  ids_alt <- json_to_df(res_1$header$extra$station$alternateids)
  ids_alt_names <- ids_alt[, names(ids_alt) == "key"]
  # ids_alt_names <- ids_alt_names[ids_alt_names != "N-dagl"] # Most likely fixed in stinfo-Facade
  ids_alt <- ids_alt[, names(ids_alt) == "id", drop = FALSE]
  colnames(ids_alt) <- ids_alt_names

  # Extract names
  name <- cbind(json_to_df(res_1$header$extra$element$name),
                json_to_df(res_1$header$extra$station$name))
  colnames(name) <- paste0(c("element", "station"), ".", "name")

  # Extract organisation selecting the latest owner(s)
  if (!is.null(res_1$header$extra$station$organisation)){
    orgs <- json_to_df(res_1$header$extra$station$organisation)
  } else {
    orgs <- data.frame(value = "NULL")
  }
  colnames(orgs) <- paste0("organisation", ".", names(orgs))

  # orgs <- matrix(orgs, ncol=3, byrow=TRUE)
  # orgs <- orgs[orgs[,3] == "0001-01-01T00:00:00Z",]
  # orgs_names <- paste0("organisation", ".", names(orgs)[1:3])
  # colnames(orgs) <- orgs_names


  # Extract quality
  quality <- json_to_df(res_1$header$extra$timeseries$quality)

  # Print station id
  print(" ")
  print("-------------------------------------------")
  print(sprintf("station %s: %s -- %s -- %s: %s",
                orgs["organisation.value"],
                ids["stationid"],
                name["station.name"],
                names(ids_alt),
                ids_alt))
  print("-------------------------------------------")
  print(" ")

  # # Extract parameter ids
  # n.paramid <- length(res$data$tseries)
  # param <- sapply(1:n.paramid,
  #                 function(x){unlist(purrr:map_dfr(res$data$tseries[[x]],
  #                                                  "id"))})[c("parameterid",
  #                                                             "sensor",
  #                                                             "level"), ]

  # Extract latest coordinates, hence rev()
  loc <- rev(json_to_df(res_1$header$extra$station$location))
  loc <- loc[c(grep("lat", names(loc))[1],
               grep("lon", names(loc))[1],
               grep("value.elev", names(loc))[1])]
  loc <- as.data.frame(t(apply(loc, 1, as.numeric)))
  names(loc) <- c("lat", "lon", "elev")

  # Build data.frame with station attributes
  stn_attrib <- cbind(ids,
                      loc,
                      loc,
                      name,
                      orgs,
                      ids_alt,
                      quality,
                      dx,
                      resx)
  if (!is.null(path)) {
    stn_attrib <- cbind(stn_attrib, path)
  }

  # Convert to SpatVector from Lat-Lon to UTM
  #-- 4326  WGS 84 / Lat Lon
  #-- 32633 WGS 84 / UTM zone 33N
  #-- 25833 ETRS89 / UTM zone 33N
  stn <- terra::vect(stn_attrib,
                     geom = c("lon", "lat"),
                     crs = "epsg:4326")
  stn <- terra::project(stn, "epsg:25833")

  return(stn)
}
## Extra help
# library(listviewer)
# jsonedit(res)
# rapply(content, class, how="list") %>% jsonedit # to view types
# df <- json_to_df(res$data$tseries[[1]])
# rownames(df) <- sub(".*extra." , "", rownames(df))
# rownames(df) <- sub(".*header.", "", rownames(df))
