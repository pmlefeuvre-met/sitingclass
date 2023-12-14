#' Sky View Factor (SVF)
#'
#' Compute Sky View Factor from horizon data
#'
#' @references \url{https://github.com/OSGeo/grass-addons/blob/grass8/src/raster/r.skyview/r.skyview.py}
#'
#' @param horizon An array of horizon height in degrees from \code{"compute_horizon"}
#'
#' @return Estimated sky view factor, 1 defines an open sky view and 0 a totally obstructed sky view
#'
#' @examples
#' compute_skyviewfactor(horizon)
#'
#' @export

compute_skyviewfactor <- function(horizon = NULL){

  # Load function
  deg2rad <- function (angle_in_degrees) {
    angle_in_radian <- (angle_in_degrees * pi)/(180)
  }

  # Convert positive angles (above horizon) in degrees to radian
  angle_above_horizon <- deg2rad(ifelse(horizon[,2] < 0, 0, horizon[,2]))
  skyviewfactor <- 1 - sum( sin(angle_above_horizon) ) / length(horizon[,1])

  return(skyviewfactor)
}
