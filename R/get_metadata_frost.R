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
#' get_latlon_frost(stationid=18700)
#' get_latlon_frost(stationid=18700,paramid=211)
#'
#' @importFrom httr2 request req_retry req_auth_basic
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom utils type.convert
#' @importFrom terra vect project
#'
#' @export

get_latlon_frost <- function(stationid = 18700,
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
    df <- as.data.frame(as.matrix(unlist(json)))
    return(df)
  }

  # Extract ids and names
  ids  <- map_dfr(res_1, "id")
  name <- map_dfr(res_1$header$extra, "name")
  colnames(name) <- paste0(colnames(name), ".", "name")

  # Extract alternative ids (WMO) and organisation
  ids_alt <- t(map_dfr(res_1$header$extra, "alternateids")["id"])
  colnames(ids_alt) <- t(map_dfr(res_1$header$extra, "alternateids")["key"])
  orgs <- map_dfr(res_1$header$extra, "organisation")
  colnames(orgs) <- paste0("organisation", ".", colnames(orgs))

  # Print station id
  cat(sprintf("station %s: %s -- %s -- WMO: %s\n",
              orgs["organisation.value"],
              ids["stationid"],
              name["station.name"],
              ids_alt[, "WMO"]))

  # # Extract parameter ids
  # n.paramid <- length(res$data$tseries)
  # param <- sapply(1:n.paramid,
  #                 function(x){unlist(map_dfr(res$data$tseries[[x]],
  #                                            "id"))})[c("parameterid",
  #                                                       "sensor",
  #                                                       "level"), ]

  # Extract latest coordinates
  loc <- cbind(map_dfr(res_1$header$extra$station$location, "value"),
               unique(map_dfr(res_1$header$extra,"location")[, c("from","to")]))
  stn_coord <- as.numeric(loc[nrow(loc), 1:3])

  # Build data.frame with station attributes
  stn_attrib <- cbind(map_dfr(res_1, "id"),
                      ids_alt,
                      map_dfr(res_1$header$extra, "name"),
                      organis,
                      t(unlist(map_dfr(res_1$header$extra, "quality"))),
                      lat = stn_coord[1],
                      lon = stn_coord[2],
                      elev = stn_coord[3])

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
