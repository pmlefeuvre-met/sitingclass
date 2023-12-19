## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo = FALSE-------------------------------------------------
#  remotes::install_git("https://YOUR_GIT_ID:YOUR_GIT_TOKEN@gitlab.met.no/pierreml/sitingclass.git",force=T)
#  .rs.restartR()
#  library(sitingclass)

## -----------------------------------------------------------------------------
library(sitingclass)

# Define station id
stationid <- 18700

# Get station metadata
stn <- get_latlon_frost(stationid)
stn

## -----------------------------------------------------------------------------
# Get coordinates
centre <- sf::st_coordinates(stn)
centre

# Construct box to extract WMS tile
dx <- 100
box <- round( c(c(centre[1],centre[2])-dx,
                c(centre[1],centre[2])+dx) )
class(box) <- "bbox"
box <- sf::st_as_sfc(box)
sf::st_crs(box) <- 25833 #32633 #to match tile projection


## ----fig.dim = c(10, 8), out.width="80%", fig.cap="A satellite image with the station location as a red dot."----
# Plot ESRI imagery
plot_tile_station(stn, box, tile_name="esri")

## ----fig.dim = c(10, 8), out.width="80%", fig.cap="Hillshade of the digital surface model."----
f.ow  <- FALSE
dem   <- download_dem_kartverket(stationid,centre,name="dtm",dx,resx=1,f.overwrite=f.ow)
dsm   <- download_dem_kartverket(stationid,centre,name="dom",dx,resx=1,f.overwrite=f.ow)
demkm <- download_dem_kartverket(stationid,centre,name="dtm",20e3,resx=20,f.overwrite=f.ow)

# Plot DEM
library(rayshader)
elmat = raster_to_matrix(dsm)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

