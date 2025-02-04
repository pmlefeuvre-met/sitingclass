#' Compute siting class for a station measuring air temperature
#'
#' Assess the class of a station from the type of surrounding land cover, slope,
#' vegetation and shadowing as potential heat sources affecting sensors and
#' using the requirements set by WMO and Met
#'
#' @references \url{https://community.wmo.int/en/activity-areas/
#' imop/siting-classification}
#'
#' @param stn A SpatVector with station attributes from
#' \code{"get_latlon_frost"}
#' @param horizon A data.frame with horizon distribution from
#' \code{"compute_horizon_max"}
#' @param test_type A string defining a type of tests to apply: "WMO" or "MET"
#' @param f_plot A boolean whether to plot figures
#'
#' @return A class
#'
#' @examples
#'# Get station metadata
#' stn <- get_metadata_frost(stationid = 18700, dx = 100, resx = 1)
#'
#' # Compute maximum horizon
#' horizon_max <- compute_horizon_max(stn, step = 1, f_plot_polygon = FALSE)
#'
#' # Compute class
#' compute_class_precipitation(stn,
#'                             horizon_max,
#'                             test_type = "WMO",
#'                             f_plot = TRUE)
#'
#'
#' @importFrom stats quantile
#' @importFrom utils stack write.csv
#'
#' @export

compute_class_precipitation <- function(stn = NULL,
                                        horizon = NULL,
                                        test_type = "MET",
                                        f_plot = TRUE) {

  ## ------------------------------- ##
  ## List of parameters to be tested
  # 3) Projected shade limits
  shade <- max(compute_horizon_rollmean(stn, horizon))
  names(shade) <- "shade"
  # 4) Compute median slope
  slope <- terra::global(terra::terrain(dem),
                         \(x) quantile(x, 0.5, na.rm = TRUE))
  names(slope) <- "slope"
  ## ------------------------------- ##

  class_names <- c("class1", "class2", "class3", "class4", "class5")
  type_names  <- c(names(shade), names(slope))
  # Set matrix of class test parameters
  if (test_type == "WMO") {
    params <- matrix(c(14, 19,
                       26, 19,
                       45, 30,
                       65, 99,
                       99, 99),
                     nrow = length(class_names),
                     ncol = length(type_names),
                     byrow = TRUE,
                     dimnames = list(class_names,
                                     type_names))
  } else if (test_type == "MET") {
    params <- matrix(c(14, 19,
                       26, 19,
                       45, 30,
                       65, 99,
                       99, 99),
                     nrow = length(class_names),
                     ncol = length(type_names),
                     byrow = TRUE,
                     dimnames = list(class_names,
                                     type_names))
  }

  # Sum area percentages of building, road and water (1:3 rows) for each
  # distance (columns) and Assign them to a matrix for assessing class
  values <- c(shade, slope)
  area <- matrix(values,
                 nrow = dim(params)[1],
                 ncol = dim(params)[2],
                 byrow = TRUE,
                 dimnames = list(rownames(params),
                                 colnames(params)))

  # Apply class tests
  col_lesser  <- which(colnames(area) %in% c(names(slope),
                                             names(shade)))
  col_greater <- which(colnames(area) %in% c(names(vegetation)))
  class_test <- cbind(area[, col_lesser]  <= params[, col_lesser],
                      area[, col_greater] >= params[, col_greater])

  # Compute number of tests per class to assess success rate per class
  test_length <- rowSums(!is.na(class_test))

  # Assess which class passed tests and success rate
  f_all <- function(x) all(x, na.rm = TRUE)
  f_per <- function(x) sum(x, na.rm = TRUE)
  final <- round(rbind(class_shade      = class_test[, names(shade)],
                       class_slope      = class_test[, names(slope)],
                       class_boolean    = apply(class_test,
                                                1, FUN = f_all),
                       success_percent  = apply(class_test,
                                                1, FUN = f_per) /
                         test_length * 100)
  )
  # print(class_test)
  # print(test_type)
  #   print(final)

  # Return assessed class name
  result <- apply(final[1:4, ], 1, FUN = function(x) names(x)[which.max(x)])
  result <- result[c(4, 2, 1, 3)]

  # Save results
  if (!is.null(stn$path)) {
    fname <- sprintf("%s/%1.0f_class_%04.0fm.csv",
                     stn$path,
                     stn$stationid,
                     stn$dx)
    utils::write.csv(result, fname)
  }

  # Print results
  print(" ")
  print("-------------------------------------------")
  print(result)
  print("-------------------------------------------")
  print("-------------------------------------------")
  print(" ")
  print(" ")
  return(result)
}
