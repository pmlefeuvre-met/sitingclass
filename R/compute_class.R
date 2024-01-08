#' Compute station class
#'
#' Assess the class of a station from the type of surrounding land cover, slope,
#' vegetation and shadowing as potential heat sources affecting sensors and
#' using the requirements set by WMO and Met
#'
#' @references \url{https://community.wmo.int/en/activity-areas/imop/siting-classification}
#'
#' @param land A data.frame with land type distribution from \code{"compute_landtype_distance"}
#' @param horizon A data.frame with horizon distribution from \code{"compute_horizon"}
#' @param dem A SpatRaster terrain model
#' @param test.type A string defining a type of tests to apply: "WMO" or "Met"
#' @param f.plot A boolean whether to plot figures
#'
#' @return A class
#'
#' @examples
#'# Get station metadata
#' stn <- get_latlon_frost(stationid=18700)
#' stn.id      <- stn$id.stationid
#' stn.centre  <- sf::st_coordinates(stn)
#' stn.level   <- stn$id.level
#'
#' # Parameters
#' dx   <- 100
#' resx <- 1
#'
#'# Compute land type
#' landtype <- compute_landtype(stn, dx, resx, f.plot=TRUE)
#'
#' # Compute land type distance to station
#' landtype_dist <- compute_landtype_distance(stn, landtype, dx, resx, f.plot=TRUE)
#'
#' # Load digital elevation models of the terrain and surface
#' dem   <- download_dem_kartverket(stn.id, stn.centre, name="dtm", dx, resx)
#' dsm   <- download_dem_kartverket(stn.id, stn.centre, name="dom", dx, resx)
#' demkm <- download_dem_kartverket(stn.id, stn.centre, name="dtm",dx=20e3,resx=20)
#'
#'   # Compute horizon view from location
#'   horizon_dem   <- compute_horizon(stn.centre,dem,level=stn.level,step=.01,f.plot.polygon=TRUE)
#'   horizon_dsm   <- compute_horizon(stn.centre,dsm,level=stn.level,step=.01,f.plot.polygon=TRUE)
#'   horizon_demkm <- compute_horizon(stn.centre,demkm,level=stn.level,step=.01,f.plot.polygon=TRUE)
#'   horizon_max   <- data.frame(azimuth=horizon_dem[,1],
#'                               horizon_height=apply(cbind(horizon_dem[,2],
#'                                                          horizon_dsm[,2],
#'                                                          horizon_demkm[,2]),
#'                                                    1, max) )
#'
#' # Compute class
#' compute_class(landtype_dist, horizon_max, dem, test.type="WMO", f.plot=TRUE)
#'
#'
#' @importFrom ggplot2 ggplot geom_area
#'
#' @export
compute_class <- function(land=landtype_dist,
                          horizon=horizon_max,
                          dem=dem,
                          test.type="WMO",
                          f.plot=TRUE){

  # Extract column and land type names
  colname <- colnames(land)
  landtype_name <- colname[-1]

  # Compute area percentage per land type
  df <- land[,colname %in% landtype_name] / land[,colname=="total_area"] *100

  # Reshape data.frame, equivalent to pivot_longer()
  df <- with(stack(as.data.frame(t( df ))),
             data.frame(distance = as.numeric(as.character(ind)),
                        landtype = factor(colnames(df), landtype_name),
                        area = values))

  if(f.plot){
    # Plot area distribution per land type in percentage and log10 x-axis
    ggplot(df, aes(x=distance, y=area, fill=landtype)) +
      geom_area(position="stack", stat="identity") +
      xlab("Distance in metre (log10 scale)") +
      ylab("Area in percentage") +
      theme_minimal() +
      scale_fill_manual(values = fill_landtype) +
      scale_x_continuous(trans = 'log10')
  }

  # Compute area within an annular area 5-10m and 10-30m
  ring <- land[rownames(land) %in% c(10,30), colname %in% landtype_name] -
    land[rownames(land) %in% c(5 ,10), colname %in% landtype_name]
  ring_area <- land[rownames(land) %in% c(10,30), colname=="total_area"] -
    land[rownames(land) %in% c(5 ,10), colname=="total_area"]
  ring <- ( ring / ring_area )*100

  # Extract areas (%) at 3, 5, 5-10, 10, 10-30, 30, 100m radius for class tests
  df.radius <- round(cbind(df[df$distance==3, "area"],
                           df[df$distance==5, "area"],
                           ring[rownames(ring)==10],
                           df[df$distance==10, "area"],
                           ring[rownames(ring)==30],
                           df[df$distance==30, "area"],
                           df[df$distance==100, "area"]))

  # Assign names for rows and columns
  rownames(df.radius) <- colnames(land)[-1]
  colnames(df.radius) <- c("3m","5m","5-10m","10m","10-30m","30m","100m")
  # print(df.radius)

  ## List of parameters to be tested
  # 1) Sum area percentages of building, road and water (1:3 rows) for each
  # distance (columns)
  landtypes <- colSums(df.radius[c("building","road","water"),])
  # 2) Sum grass to crop area and compute mean over distance classes
  vegetation     <- df.radius[c("grass","crop"),]
  vegetation[2,] <- colSums(vegetation)
  vegetation     <- round( rowMeans(vegetation))
  # 3) Projected shade limits
  shade <- quantile(horizon[,"horizon_height"],.90)
  names(shade) <- "shade"
  # 4) Compute median slope
  slope <- terra::global(terra::terrain(dem),\(x) quantile(x,0.5,na.rm=T))
  names(slope) <- "slope"

  # Set matrix of class test parameters
  if(test.type=="WMO"){
    class.names <- c("class1","class2","class3","class4","class5")
    type.names  <- c(names(landtypes), names(vegetation),
                     names(shade), names(slope))
    params <- matrix(c(NA,NA,NA, 1, 5,NA,10,51, 0, 5,19,
                       NA, 1, 5,NA,NA,10,NA,51, 0, 7,19,
                       NA, 5,NA,10,NA,NA,NA,99,51, 7,99,
                       30,NA,NA,50,NA,NA,NA,99,51,20,99,
                       NA,NA,NA,NA,NA,NA,NA,99,99,99,99),
                     nrow = length(class.names),
                     ncol = length(type.names),
                     byrow = T,
                     dimnames = list(class.names,
                                     type.names) )
  }

  # Sum area percentages of building, road and water (1:3 rows) for each
  # distance (columns) and Assign them to a matrix for assessing class
  values <- c(landtypes, vegetation, shade, slope)
  area <- matrix(values,
                 nrow = dim(params)[1],
                 ncol = dim(params)[2],
                 byrow = T,
                 dimnames = list(rownames(params),
                                 colnames(params)) )

  # Apply class tests
  col.lesser  <- which(colnames(area) %in% c(names(landtypes ),
                                             names(slope),
                                             names(shade)))
  col.greater <- which(colnames(area) %in% c(names(vegetation)))
  class.test <- cbind( area[,col.lesser ] <= params[,col.lesser ],
                       area[,col.greater] >= params[,col.greater])

  # Compute number of tests per class to assess success rate per class
  test.length <- rowSums(!is.na(class.test))

  # Assess which class passed tests and success rate
  f.all <- function(x) all(x,na.rm = TRUE)
  f.per <- function(x) sum(x,na.rm = TRUE)
  final <- round(rbind(class_landtype  =apply(class.test[,names(landtypes)] , 1, FUN = f.all),
                       class_vegetation=apply(class.test[,names(vegetation)], 1, FUN = f.all),
                       class_shade     =class.test[,names(shade)],
                       class_slope     =class.test[,names(slope)],
                       class_boolean   =apply(class.test, 1, FUN = f.all),
                       success_percent =apply(class.test, 1, FUN = f.per)/test.length*100 ))
  # print(class.test)
  # print(test.type)
  # print(final)

  # Return assessed class name
  return( apply(final[1:4,], 1, FUN = function(x) names(x)[which.max(x)]) )
}
