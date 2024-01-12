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
#' @importFrom sf st_as_sf st_crs st_transform
#' @importFrom httr authenticate GET content
#' @importFrom utils type.convert
#'
#' @export

get_latlon_frost <- function(stationid = 18700,
                             paramid = NULL) {

  # Define frost url
  url <- "https://frost-beta.met.no/api/v1/obs/met.no/filter/get?"
  url <- sprintf("%sincobs=false&stationids=%i&", url, stationid)
  if (!is.null(paramid)) {url <- sprintf("%sparameterids=%i&", url, paramid)}
  url <- sprintf("%sbasicoutput=false&time=latest", url)
  auth <- authenticate(Sys.getenv("FROST_ID"), Sys.getenv("FROST_KEY"))

  # Query URL
  res     <- GET(url, auth)

  # Format response content as a data.frame
  content     <- content(res, )#     str(content)
  if (length(grep("error", content)) > 0) {print(content); stop()}
  n.paramid <- length(content$data$tseries)
  df <- content$data$tseries[[1]] %>% unlist() %>% as.matrix() %>% as.data.frame()
  rownames(df) <- sub(".*extra." , "", rownames(df))
  rownames(df) <- sub(".*header.", "", rownames(df))
  # unlist %>% tail(-1) %>% as.matrix

  # Print station id
  cat(sprintf("station: %s \t %s\n",
              df["id.stationid", ],
              df["station.name", ]))

  # Extract parameter ids
  param <- sapply(1:n.paramid,
                  function(x){unlist(content$data$tseries[[x]]$header$id)})[c("parameterid", "sensor", "level"), ]
  # elem <- sapply(1:n.paramid,
  #                       function(x){unlist(content$data$tseries[[x]]$header$extra$element)})[[1]][c("id","name","elementcodes")]
  # if(n.paramid==1){
  #   cat(sprintf("\t-paramid: %s \t %s\n",param["parameterid"],elem["name"]))
  # }else{
  #   cat(sprintf("\t-paramid: %s \t %s\n",param["parameterid",],elem["name",]))
  # }
  # if(n.paramid==1){ cat(sprintf("\t-paramid: %s\n",param["parameterid"]))
  # }else{              cat(sprintf("\t-paramid: %s\n",param["parameterid",])) }

  # Extract latest coordinates
  l_loc <- length(content$data$tseries[[1]]$header$extra$station$location)
  stn_coord <- as.numeric(unlist(content$data$tseries[[1]]$header$extra$station$location[[l_loc]]$value))

  # # Transform from lat lon to utm
  # stn <- stn_coord[c(2:1,3)] %>% st_point() %>% st_sfc(crs=st_crs(4326)) %>% st_transform(25833) #32633
  rows <- c(grep("id.*", rownames(df)),
            grep("station.name*", rownames(df)),
            grep("organisation.*", rownames(df)),
            grep("level.*", rownames(df)),
            grep("quality.*", rownames(df)))
  stn_attrib <- df[rows, , drop = FALSE] %>% t %>% cbind(lat = stn_coord[1],
                                                         lon = stn_coord[2],
                                                         elev = stn_coord[3])
  colnames <- colnames(stn_attrib)
  stn_attrib <- data.frame( lapply(split(stn_attrib, col(stn_attrib)),
                                   utils::type.convert,
                                   as.is = TRUE),
                            stringsAsFactors = FALSE )
  colnames(stn_attrib) <- colnames
  stn <- sf::st_as_sf(stn_attrib, coords=c("lon", "lat"),
                      crs=sf::st_crs(4326)) %>% sf::st_transform(25833) #32633
  sf::st_crs(stn) <- 25833 # 32633 - WGS 84 / UTM zone 33N # 25833 ETRS89 / UTM zone 33N

  return(stn)

  ## Extra help
  # str(content$data$tseries[[1]]$header$extra$station)
  # library(listviewer)
  # jsonedit(content)
  # rapply(content, class, how="list") %>% jsonedit # to view types
  #
  #   url <- "https://frost-beta.met.no/api/v1/obs/met.no/filter/get?"
  # url <- sprintf("%sincobs=false&stationids=%i&parameterids=%i&",url,stationid,paramid)
  # url_all <- sprintf("%sbasicoutput=false&time=latest",url)
  # url_loc <- sprintf("%shdrshow={"extra":{"station":{"location":[]}}}",url)
  # url_lev <- sprintf("%shdrshow={"id":{"level":{}}}",url)
  # url_lev <- sprintf("%shdrshow={"extra":{"timeseries":{"geometry":{}}}}",url)
  # url_QA  <- sprintf("%shdrshow={"extra":{"timeseries":{"quality":{}}}}",url)
  # auth <- authenticate("ea623856-933a-4bcd-ac39-80b1d30ab6f8","ca0050c5-1112-45db-8114-52caee6967bb")
  # # Query URL
  # res     <- GET(url,auth)
  # res_loc <- GET(url_loc,auth)
  # res_lev <- GET(url_lev,auth)
  # res_QA  <- GET(url_QA,auth)
  # # Extract response content and coordinates
  # content     <- content(res,);     str(content)
  # content_loc <- content(res_loc,); str(content_loc)
  # content_lev <- content(res_lev,); str(content_lev); cat(unlist(content_lev))
  # content_QA  <- content(res_QA,);  str(content_QA)
  # df_lev <- content(GET(url_lev,auth),) %>% unlist %>% tail(-1) %>% as.matrix %>% as.data.frame
  # df_loc <- content(GET(url_loc,auth),) %>% unlist %>% tail(-1) %>% as.matrix %>% as.data.frame
  # rownames(df_lev) <- sub(".*.level.","",rownames(df_lev))
  # rownames(df_loc) <- sub(".*.location.","",rownames(df_loc))
}
