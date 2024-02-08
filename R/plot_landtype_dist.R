#' Plot area-landtype distribution
#'
#' Plot distribution of landtype area with distance from a weather station to
#' asses the surrounding environment and potnetial impact on the measurements
#'
#' @param stn A SpatVector with station attributes from
#' \code{"get_latlon_frost"}
#' @param land A data.frame with land type distribution from
#' \code{"compute_landtype_distance"}
#'
#' @return A figure
#'
#' @examples
#'# Example of input to plot
#'# plot_landtype_dist(stn, landtype_dist)
#'
#' @importFrom ggplot2 ggplot geom_area
#' @importFrom utils stack
#'
#' @export
plot_landtype_dist <- function(stn = NULL,
                               land = NULL) {

  # Bind variable to function
  distance <- landtype <- NULL
  land <- landtype_dist

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

  # Plot area distribution per land type in percentage and log10 x-axis
  g <- ggplot(df, aes(x = distance, y = area, fill = landtype)) +
    geom_area(position = "stack", stat = "identity") +
    xlab("Distance in metre (log10 scale)") +
    ylab("Area in percentage") +
    theme_minimal() +
    scale_fill_manual(values = fill_landtype) +
    scale_x_continuous(trans = "log10",
                       minor_breaks = c(1:9, 1:9*10, 1:9*100, 1:9*1000))
  if (is.null(stn$path)) {
    print(g)
  } else {
    fname <- sprintf("%s/%i_landtype_area_%04.0fm.png",
                     stn$path,
                     stn$stationid,
                     stn$dx)
    ggsave(fname, bg = "white", width = 7, height = 7)
  }

  return(g)
}




