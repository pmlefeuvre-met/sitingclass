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
#'
#' @return Station metadata
#'
#' @examples
#' get_metadata_frost(stationid=18700)
#' get_metadata_frost(stationid=18700,paramid=211)
#'
#' @importFrom httr2 request req_retry req_auth_basic
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom utils type.convert
#' @importFrom terra vect project
#'
#' @export

get_metadata_frost <- function(stationid = 18700,
                               paramid = NULL) {

  # Define Frost URL
  url <- "https://frost-beta.met.no/api/v1/obs/met.no/filter/get?"
  url <- sprintf("%sincobs=false&stationids=%i&", url, stationid)
  if (!is.null(paramid)) {
    url <- sprintf("%sparameterids=%i&", url, paramid)
  }
  url <- sprintf("%sbasicoutput=false&time=latest", url)

  # Build request and authentication
  req <- httr2::request(url) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_auth_basic(Sys.getenv("FROST_ID"), Sys.getenv("FROST_KEY"))

  # Get response as json
  res <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  res_1 <- res$data$tseries[[1]]

  # Function to convert json to dataframe
  json_to_df <- function(json) {
    df <- as.data.frame(t(as.matrix(unlist(json))))
    return(df)
  }

  # Extract ids, names and organisation
  ids  <- json_to_df(res_1$header$id)

  # Extract alternative ids (WMO,...)
  ids_alt <- json_to_df(res_1$header$extra$station$alternateids)
  ids_alt_names <- ids_alt[, names(ids_alt) == "key"]
  ids_alt_names <- ids_alt_names[ids_alt_names != "N-dagl"]
  ids_alt <- ids_alt[, names(ids_alt) == "id", drop = F]
  colnames(ids_alt) <- ids_alt_names

  # Extract names
  name <- cbind(json_to_df(res_1$header$extra$element$name),
                json_to_df(res_1$header$extra$station$name))
  colnames(name) <- paste0(c("element", "station"), ".", "name")

  # Extract organisation
  orgs <- json_to_df(res_1$header$extra$station$organisation)
  colnames(orgs) <- paste0("organisation", ".", names(orgs))

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
  loc <- as.data.frame(t(apply(loc, 1,as.numeric)))
  names(loc) <- c("lat","lon","elev")

  # Build data.frame with station attributes
  stn_attrib <- cbind(ids,
                      ids_alt,
                      name,
                      orgs,
                      quality,
                      loc)

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

