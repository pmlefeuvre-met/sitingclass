#' Convert JSON to dataframe
#'
#' Unlist JSON to matrix, then transpose and convert to dataframe
#'
#' @param json A JSON object
#'
#' @return A data.frame
#'
#' @examples
#' #No example
#'
#' @export

json_to_df <- function(json) {
  df <- as.data.frame(t(as.matrix(unlist(json))))
  return(df)
}


#' Extract and format JSON metadata
#'
#' Extract and format JSON metadata such as: level, parameterid, sensor,
#' stationid, alternative ids (WIGOS, ...) and latest coordinates.
#'
#' @param res_1 A JSON object from an individual station or parameterid
#'
#' @return A data.frame
#'
#' @examples
#' #No example
#'
#' @export

format_resp <- function(res_1) {

  ## Extract id infos: level, parameterid, sensor, stationid
  ids  <- json_to_df(res_1$header$id)

  ## Extract alternative ids (WIGOS, ...)
  # Check if alternateids is not empty and contains WIGOS key
  if (!is.null(res_1$header$extra$station$alternateids) &&
      any("WIGOS" %in% unlist(res_1$header$extra$station$alternateids))) {

    # Convert to dataframe and format: key as name and id as value
    ids_alt <- json_to_df(res_1$header$extra$station$alternateids)
    ids_alt_names <- ids_alt[, names(ids_alt) == "key"]
    ids_alt <- ids_alt[, names(ids_alt) == "id", drop = FALSE]
    colnames(ids_alt) <- ids_alt_names
  } else {
    # Assign empty dataframe if there are no values
    ids_alt <- data.frame(WIGOS = "NA")
  }

  ## Extract latest coordinates, hence rev()
  loc <- rev(json_to_df(res_1$header$extra$station$location))
  loc <- loc[c(grep("lat", names(loc))[1],
               grep("lon", names(loc))[1],
               grep("value.elev", names(loc))[1])]
  loc <- as.data.frame(t(apply(loc, 1, as.numeric)))
  names(loc) <- c("lat", "lon", "elev")

  # Build data.frame with station attributes
  stn_attrib <- cbind(ids,
                      ids_alt["WIGOS"],
                      loc)

  return(stn_attrib)
}



#' Loop through request responses
#'
#' Loop through request responses from pagination to extract metadata
#'
#' @param resp A request response
#'
#' @return A matrix
#'
#' @importFrom httr2 resp_body_json
#'
#' @examples
#' #No example
#'
#' @export

extract_resp <- function(resp) {

  # Convert response to json
  res <- httr2::resp_body_json(resp)

  # Extract the number of elements, i.e. metadata/time series in the response
  n_paramid <- length(res$data$tseries)

  # Loop through each response element
  dat <- t(sapply(1:n_paramid,
                  function(x) format_resp(res$data$tseries[[x]]),
                  simplify = "matrix"))

  return(dat)
}
