% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tile_wms.R
\name{get_tile_wms}
\alias{get_tile_wms}
\title{Get Web Mapping Service data tiles}
\usage{
get_tile_wms(box = NULL, layer = "ar5", px = 500)
}
\arguments{
\item{box}{A SpatExtent defining the area to plot}

\item{layer}{A name of a layer provided by WMS such as "ar5", "toporaster",
"CORINE_Land_Cover_2012", "Urban_Atlas_Lu_Lc_2012","ortofoto",
"fkb_arealdekke", "fkb_arealdekke","fkb_vann","vann_omrade",
"fkb_samferdsel", "veg","fkb_bygning","bygning","fkb_naturinfo",
"naturinfo"}

\item{px}{A pixel number that defines the resolution of the image/tile,
default is 500 px}
}
\value{
A map tile
}
\description{
Fetch map tiles from publicly available WMS released by Nibio
}
\examples{
# Get station coordinates and name
stn <- get_metadata_frost(stationid = 18700, dx = 100)

# Construct box to extract WMS tile
box <- make_bbox(stn)

# Load tiles
#tile <- get_tile_wms(box, layer = "ar5")
tile <- get_tile_wms(box, layer = "CORINE_Land_Cover_2012")
#tile <- get_tile_wms(box, layer = "Urban_Atlas_Lu_Lc_2012")
#tile <- get_tile_wms(box, layer = "ortofoto")
tile <- get_tile_wms(box, layer = "fkb_vann")

}
\references{
\url{https://nibio.no/tjenester/wms-tjenester}
}
