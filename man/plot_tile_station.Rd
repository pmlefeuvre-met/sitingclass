% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tile_station.R
\name{plot_tile_station}
\alias{plot_tile_station}
\title{Plot station location with background WMS tiles}
\usage{
plot_tile_station(
  stn = NULL,
  box = NULL,
  tile_name = "osm",
  dsm = NULL,
  path = stn$path
)
}
\arguments{
\item{stn}{A SpatVector with station attributes from
\code{"get_latlon_frost"}}

\item{box}{A SpatExtent defining the area to plot}

\item{tile_name}{A string defining the type of tile to plot among "osm"
(map, default), "esri" (satellite imagery), "ar5" (area type), "clc"
(Corine land cover) and "urban" (urban atlas)}

\item{dsm}{A SpatRaster of a digital surface model around the station,
expected radius is 100 m}

\item{path}{A string path that defines where to save the plot, if NULL
(default) the plot is printed on-screen and not saved}
}
\value{
A ggplot object
}
\description{
Plots publicly-available maps, atlas, land cover or satellite imagery near
a weather station based on maptiles and custom-made \code{"get_tile_wms"}
}
\examples{
# Get station coordinates and name
stn    <- get_metadata_frost(stationid = 18700, dx = 100, resx = 1)

# Construct box to extract WMS tile
box <- make_bbox(stn)

# Plot maps using plot_tile_station()
g <- plot_tile_station(stn, box, tile_name = "esri")
g
plot_tile_station(stn, box, tile_name = "esri")
plot_tile_station(stn, box, tile_name = "ar5")
plot_tile_station(stn, box, tile_name = "clc")
plot_tile_station(stn, box, tile_name = "urban")

# Include Digital Elevation Model as contour
dsm   <- download_dem_kartverket(stn, name = "dom")
plot_tile_station(stn, box, tile_name = "osm", dsm = dsm, path = "plot/map")

}
\references{
\url{https://github.com/riatelab/maptiles}
}
