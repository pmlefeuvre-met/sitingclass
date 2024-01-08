#' Compute station class for landtypes
#'
#' Assess the class of a station from the type of land cover around a station
#' by applying WMO's station test
#'
#' @references \url{https://community.wmo.int/en/activity-areas/imop/siting-classification}
#'
#' @param df.in A data.frame with land type distribution from \code{"compute_landtype_distance"}
#' @param test.type A string defining a type of tests to apply: "WMO" or "Met"
#' @param f.plot A boolean whether to plot figures
#'
#' @return A class
#'
#' @examples
#'# Get station metadata
#' stn <- get_latlon_frost(stationid=18700)
#'
#'# Compute land type
#' landtype <- compute_landtype(stn, dx=100, f.plot=TRUE)
#'
#' # Compute land type distance to station
#' landtype_dist <- compute_landtype_distance(stn, landtype, dx=100, f.plot=TRUE)
#'
#' # Compare theoretical Area and from the output (Raster rounding error: 0%)
#' plot(rep(as.numeric(rownames(landtype_dist)),2),landtype_dist[,1:2])
#' summary((landtype_dist[,1]-landtype_dist[,2])/landtype_dist[,2]*100)
#'
#'
#' @importFrom sf st_coordinates
#' @importFrom terra vect mask
#' @importFrom ggplot2 ggplot scale_fill_manual coord_sf theme_minimal
#' @importFrom tidyterra geom_spatvector
#'
#' @export
compute_class_landtype <- function(df.in,
                                   test.type="WMO",
                                   f.plot=TRUE){
  # Colour parameter
  fill <- c("building"="skyblue3",
            "road"="azure3",
            "water"="cadetblue2",
            "grass"="darkolivegreen1",
            "bush"="darkolivegreen3",
            "tree"="chartreuse4")

  # Extract column and land type names
  colname <- colnames(df.in)
  landtype_name <- colname[-1]

  # Compute area percentage per land type
  df <- df.in[,colname %in% landtype_name] / df.in[,colname=="total_area"] *100

  # Reshape data.frame, equivalent to pivot_longer()
  df <- with(stack(as.data.frame(t( df ))),
             data.frame(distance = as.numeric(as.character(ind)),
                        landtype = factor(colnames(df), landtype_name),
                        area = values))

  if(f.plot){
    # Plot area distribution per land type in percentage and log10 x-axis
    ggplot(df_percent, aes(x=distance, y=area, fill=landtype)) +
      geom_area(position="stack", stat="identity") +
      xlab("Distance in metre (log10 scale)") +
      ylab("Area in percentage") +
      theme_minimal() +
      scale_fill_manual(values = fill) +
      scale_x_continuous(trans = 'log10')
  }

  # Compute area within an annular area 5-10m and 10-30m
  ring <- df.in[rownames(df.in) %in% c(10,30), colname %in% landtype_name] -
    df.in[rownames(df.in) %in% c(5 ,10), colname %in% landtype_name]
  ring_area <- df.in[rownames(df.in) %in% c(10,30), colname=="total_area"] -
    df.in[rownames(df.in) %in% c(5 ,10), colname=="total_area"]
  ring <- ( ring / ring_area )*100

  # Extract areas at 3, 5, 5-10, 10, 10-30, 30, 100m radius for class tests
  df.radius <- round(cbind(df[df$distance==3,3],
                           df[df$distance==5,3],
                           ring[rownames(ring)==10],
                           df[df$distance==10,3],
                           ring[rownames(ring)==30],
                           df[df$distance==30,3],
                           df[df$distance==100,3]))

  # Assign names for rows and columns
  rownames(df.radius) <- colnames(df.in)[-1]
  colnames(df.radius) <- c("3m","5m","5-10m","10m","10-30m","30m","100m")
  print(df.radius)

  # Set matrix of class test parameters
  if(test.type=="WMO"){
    class.names <- c("class1","class2","class3","class4","class5")
    params <- matrix(c(NA,NA,NA, 1, 5,NA,10,
                       NA, 1, 5,NA,NA,10,NA,
                       NA, 5,NA,10,NA,NA,NA,
                       30,NA,NA,50,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA),
                     nrow = length(class.names),
                     ncol = dim(df.radius)[2],
                     byrow = T,
                     dimnames = list(class.names,
                                     colnames(df.radius)) )
  }

  # Sum area percentages of building, road and water (1:3 rows) for each
  # distance (columns) and Assign them to a matrix for assessing class
  area <- matrix(colSums(df.radius[1:3,]),
                 nrow = dim(params)[1],
                 ncol = dim(df.radius)[2],
                 byrow = T)

  # Apply class tests
  class.test <- ( area <= params )

  # Compute number of tests per class to assess success rate per class
  test.length <- rowSums(!is.na(class.test))

  # Assess which class passed tests and success rate
  final <- round(rbind(class_boolean  =apply(class.test, 1, FUN = function(x) all(x,na.rm = TRUE)),
                       success_percent=apply(class.test, 1, FUN = function(x) sum(x,na.rm = TRUE))/test.length*100))
  print(test.type)
  print(final)

  # Return assessed class name
  return( rownames(class.test)[which.max(class.test)] )
}
