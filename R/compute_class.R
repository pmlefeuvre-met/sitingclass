#' Compute station class
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
#' @param land A data.frame with land type distribution from
#' \code{"compute_landtype_distance"}
#' @param horizon A data.frame with horizon distribution from
#' \code{"compute_horizon_max"}
#' @param dem A SpatRaster terrain model
#' @param test_type A string defining a type of tests to apply: "WMO" or "Met"
#' @param f_plot A boolean whether to plot figures
#'
#' @return A class
#'
#' @examples
#'# Get station metadata
#' stn <- get_metadata_frost(stationid = 18700, dx = 100, resx = 1)
#'
#' # Load a digital elevation model
#' dem   <- download_dem_kartverket(stn, name = "dtm")
#'
#'# Compute land type
#' landtype <- compute_landtype(stn, f_plot = TRUE)
#'
#' # Compute land type distance to station
#' landtype_dist <- compute_landtype_distance(stn, landtype, f_plot = TRUE)
#'
#' # Compute maximum horizon
#' horizon_max <- compute_horizon_max(stn, step = 1, f_plot_polygon = FALSE)
#'
#' # Compute class
#' compute_class(stn, landtype_dist, horizon_max, dem, test_type = "WMO",
#'               f_plot = TRUE)
#'
#'
#' @importFrom stats quantile
#' @importFrom utils stack write.csv
#'
#' @export

compute_class <- function(stn = NULL,
                          land = NULL,
                          horizon = NULL,
                          dem = NULL,
                          test_type = "WMO",
                          f_plot = TRUE) {

  # Bind variable to function
  distance <- landtype <- NULL

  # Extract column and land type names
  colname <- colnames(land)
  landtype_name <- colname[-1]

  # Compute area percentage per land type
  df <- land[, colname %in% landtype_name] /
    land[, colname == "total_area"] * 100

  # Reshape data.frame, equivalent to pivot_longer()
  df <- with(utils::stack(as.data.frame(t(df))),
             data.frame(distance = as.numeric(as.character(ind)),
                        landtype = factor(colnames(df), landtype_name),
                        area = values))

  if (f_plot) {
    plot_landtype_dist(stn, land = land)
  }

  # Compute area within an annular area 5-10m and 10-30m
  ring <- land[rownames(land) %in% c(10, 30), colname %in% landtype_name] -
    land[rownames(land) %in% c(5, 10), colname %in% landtype_name]
  ring_area <- land[rownames(land) %in% c(10, 30), colname == "total_area"] -
    land[rownames(land) %in% c(5, 10), colname == "total_area"]
  ring <- (ring / ring_area) * 100

  # Extract areas (%) at 3, 5, 5-10, 10, 10-30, 30, 100m radius for class tests
  df_radius <- round(cbind(df[df$distance == 3, "area"],
                           df[df$distance == 5, "area"],
                           ring[rownames(ring) == 10],
                           df[df$distance == 10, "area"],
                           ring[rownames(ring) == 30],
                           df[df$distance == 30, "area"],
                           df[df$distance == 100, "area"]))

  # Assign names for rows and columns
  rownames(df_radius) <- colname[-1]
  colnames(df_radius) <- c("3m", "5m", "5-10m", "10m", "10-30m", "30m", "100m")

  ## List of parameters to be tested
  # 1) Sum area percentages of building, road and water (1:3 rows) for each
  # distance (columns)
  landtypes <- colSums(df_radius[colname[-1] %in%
                                   c("building", "road", "water"), ])
  # 2) Sum grass to crop area and compute mean over distance classes
  vegetation      <- df_radius[colname[-1] %in% c("grass", "crop"), ]
  vegetation[2, ] <- colSums(vegetation)
  vegetation      <- round(rowMeans(vegetation))
  # 3) Projected shade limits
  shade <- max(compute_horizon_rollmean(stn, horizon))
  names(shade) <- "shade"
  # 4) Compute median slope
  slope <- terra::global(terra::terrain(dem),
                         \(x) quantile(x, 0.5, na.rm = TRUE))
  names(slope) <- "slope"

  # Set matrix of class test parameters
  if (test_type == "WMO") {
    class_names <- c("class1", "class2", "class3", "class4", "class5")
    type_names  <- c(names(landtypes), names(vegetation),
                     names(shade), names(slope))
    params <- matrix(c(NA, NA, NA,  1,  5, NA, 10, 51,  0,  5, 19,
                       NA, 1,  5,  NA, NA, 10, NA, 51,  0,  7, 19,
                       NA, 5,  NA, 10, NA, NA, NA, 99, 51,  7, 99,
                       30, NA, NA, 50, NA, NA, NA, 99, 99, 20, 99,
                       NA, NA, NA, NA, NA, NA, NA, 99, 99, 99, 99),
                     nrow = length(class_names),
                     ncol = length(type_names),
                     byrow = TRUE,
                     dimnames = list(class_names,
                                     type_names))
  } else if (test_type == "MET") {
    class_names <- c("class1", "class2", "class3", "class4", "class5")
    type_names  <- c(names(landtypes), names(vegetation),
                     names(shade), names(slope))
    params <- matrix(c(NA, NA, NA,  1,  5, NA, 10, 51,  0,  7, 19,
                       NA, 1,  5,  NA, NA, 10, NA, 51,  0,  7, 19,
                       NA, 5,  NA, 10, NA, NA, NA, 99, 51,  7, 99,
                       30, NA, NA, 50, NA, NA, NA, 99, 51, 20, 99,
                       NA, NA, NA, NA, NA, NA, NA, 99, 99, 99, 99),
                     nrow = length(class_names),
                     ncol = length(type_names),
                     byrow = TRUE,
                     dimnames = list(class_names,
                                     type_names))
  }

  # Sum area percentages of building, road and water (1:3 rows) for each
  # distance (columns) and Assign them to a matrix for assessing class
  values <- c(landtypes, vegetation, shade, slope)
  area <- matrix(values,
                 nrow = dim(params)[1],
                 ncol = dim(params)[2],
                 byrow = TRUE,
                 dimnames = list(rownames(params),
                                 colnames(params)))

  # Apply class tests
  col_lesser  <- which(colnames(area) %in% c(names(landtypes),
                                             names(slope),
                                             names(shade)))
  col_greater <- which(colnames(area) %in% c(names(vegetation)))
  class_test <- cbind(area[, col_lesser]  <= params[, col_lesser],
                      area[, col_greater] >= params[, col_greater])

  # Compute number of tests per class to assess success rate per class
  test_length <- rowSums(!is.na(class_test))

  # Assess which class passed tests and success rate
  f_all <- function(x) all(x, na.rm = TRUE)
  f_per <- function(x) sum(x, na.rm = TRUE)
  final <- round(rbind(class_landtype   = apply(class_test[, names(landtypes)],
                                                1, FUN = f_all),
                       class_vegetation = apply(class_test[, names(vegetation)],
                                                1, FUN = f_all),
                       class_shade      = class_test[, names(shade)],
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
