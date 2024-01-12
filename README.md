# R package `sitingclass`
The `sitingclass` package computes the exposure of a weather station. Currently, the package is built for Norwegian weather stations that are maintained by the Norwegian Meteorological Institute (aka Met). 

## Startup
### Installation
The package `sitingclass` is available on Met's Gitlab and can be installed using `remotes::install_git()`. Send an access request to [pierre.marie.lefeuvre@met.no](mailto:pierre.marie.lefeuvre@met.no) to get `YOUR_GIT_ID` and `YOUR_GIT_TOKEN`. See [Authentication](#Authentication) on how to make your id and token accessible to your R environment.

```R
# manually adding your token
remotes::install_git("https://YOUR_GIT_ID:YOUR_GIT_TOKEN@gitlab.met.no/pierreml/sitingclass.git",force=T)

# If you have set your token in .Renviron, use:
remotes::install_git(paste0("https://",Sys.getenv('GIT_ID'),":",Sys.getenv('GIT_TOKEN'),"@gitlab.met.no/pierreml/sitingclass.git"),force=T)

.rs.restartR()
library(sitingclass)
```

### Authentication
Met's Frost API is used to retrieve the necessary station metadata including coordinates and station name. First [Register as a user](https://frost-beta.met.no/docs/starthere) to obtain `YOUR_FROST_ID` and `YOUR_FROST_KEY`. To make them accessible to your R environment, save them in a `.Renviron` file (as documented in [rstats/R-startup](https://rstats.wtf/r-startup.html)), such as:
```bash
cd $HOME
cat > .Renviron
GIT_ID=YOUR_GIT_ID
GIT_TOKEN=YOUR_GIT_TOKEN
FROST_ID=YOUR_FROST_ID
FROST_KEY=YOUR_FROST_KEY
```
The function `get_metadata_frost()` will fetch `FROST_ID` and `FROST_KEY` then setting `authenticate()` for the URL request.

### Dependencies
`sitingclass` depends on the following packages:

* Data and metadata retrieval:
  + [httr](https://httr.r-lib.org/)/[httr2](https://httr2.r-lib.org/): to get station metadata using **URL get request** (i.e. Frost)
  + [maptiles](https://github.com/riatelab/maptiles): to get tiles from available map providers
  + [ows4R](https://github.com/eblondel/ows4R): to download DEMs using **WCS getCoverage** (Web Coverage Service)
* Geospatial processing:
  + [sf](https://github.com/rspatial/sf): vector processing and conversion
  + [terra](https://github.com/rspatial/terra): SpatRaster and SpatVector processing and conversion
* Computing horizon and sun position:
  + [rgrass](https://rsbivand.github.io/rgrass/): to compute horizon from GRASS
  + [suntools](https://github.com/adokter/suntools/): to compute sun position in the sky
* Plotting:
  + [ggplot2](https://ggplot2.tidyverse.org/): plotting functions
  + [rayshader](https://www.rayshader.com/): to plot DEMs with shaded relief and in 3D
  + [tidyterra](https://dieghernan.github.io/tidyterra/): to plot raster images and contour
* Utils:
  + [magrittr](https://magrittr.tidyverse.org/): pipping (i.e. `%>%`)
  
### Data and metadata 
`sitingclass` aims to be portable retrieving most of the needed data sets from public web services, including:

- [Frost](https://frost-beta.met.no/docs/codeexamples): station metadata
- [Kartverket](https://www.kartverket.no/geodataarbeid/nasjonal-detaljert-hoydemodell)/[Geonorge](https://kartkatalog.geonorge.no/metadata?text=25833+WCS+h%C3%B8ydemodell): digital terrain model, digital surface model and National map database [FKB](https://kartkatalog.geonorge.no/metadata/geovekst/felles-kartdatabase-fkb/0e90ca71-6a02-4036-bd94-f219fe64645f) in UTM 33 (epsg:25833)
- [Nibio](https://nibio.no/tjenester/wms-tjenester/wms-tjenester-ar5): WMS (Web Map Services) Land cover maps

## Usage
The following examples are also accessible from the vignette `sitingclass`. From R, load the library and run: 
```
vignette("sitingclass", package = "sitingclass")
```
### Get the station's metadata

```
library(sitingclass)

# Define station id
stationid <- 18700

# Get station metadata
stn <- get_latlon_frost(stationid)
stn
```
### Construct the coordinate box
```
# Get coordinates
centre <- sf::st_coordinates(stn)
centre

# Construct box to extract WMS tile
dx <- 100
box <- make_bbox(centre,dx)
```

```
# Plot ESRI imagery tile
plot_tile_station(stn, box, tile_name="esri")
```

```
# Plot tile with grid and proximity circles 
plot_station_grid(stn, tile_name="ortofoto")
```

### Download Digital Elevation Models
```
# Load Karverket's DEMs
f.ow  <- FALSE #TRUE to overwrite files
dem   <- download_dem_kartverket(stn,name="dtm",dx  ,resx=1 ,f.overwrite=f.ow)
dsm   <- download_dem_kartverket(stn,name="dom",dx  ,resx=1 ,f.overwrite=f.ow)
demkm <- download_dem_kartverket(stn,name="dtm",20e3,resx=20,f.overwrite=f.ow)

# Plot 100-m Digital Surface Model and 20-km Digital Terrain Model
library(rayshader)
elmat   <- raster_to_matrix(dsm)
elmatkm <- raster_to_matrix(demkm)

# Shaded relief for the 100-m Digital Surface Model 
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

# Shaded relief for the 20-km Digital Terrain Model
elmatkm %>%
  sphere_shade(texture = "desert") %>%
  plot_map()
```
### Compute and plot the horizon and sun diagram
```
plot_station_horizon_sun(stn, dem, dsm, demkm)
```

## `plot_station_siting_context()`
The function `plot_station_siting_context()` will compute all the code chunks from [Usage] such as:

```
# Set timezone to avoid time shift between winter and summer time
Sys.setenv(TZ="UTC") # "Europe/Oslo"

# The main function plotting sun diagram and context for a weather station 
plot_station_siting_context(stationid=18700, paramid=211, f.verbose=F, f.pdf=F)
```

