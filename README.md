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
Met's Frost API is used to retrieve the necessary station metadata including coordinates and station name. First [Register as a user](https://frost-beta.met.no/docs/starthere) to obtain `YOUR_ID` and `YOUR_KEY`. To make them accessible to your R environment, save them in a `.Renviron` file (more info [here](https://rstats.wtf/r-startup.html)), such as:
```bash
cd $HOME
cat > .Renviron
GIT_ID=YOUR_GIT_ID
GIT_TOKEN=YOUR_GIT_TOKEN
FROST_ID=YOUR_FROST_ID
FROST_KEY=YOUR_FROST_KEY
```
The function `get_metadata_frost()` will fetch `FROST_ID` and `FROST_KEY` to set `authenticate()` for the URL request.

### Dependencies
`sitingclass` depends on the following packages:

* Data and metadata retrieval:
  + [httr](https://httr.r-lib.org/)/[httr2](https://httr2.r-lib.org/): to get station metadata using **URL get request** (i.e. Frost)
  + [maptiles](https://github.com/riatelab/maptiles): to get tiles from available map provides
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
- [Kartverket](https://www.kartverket.no/geodataarbeid/nasjonal-detaljert-hoydemodell)/[Geonorge](https://kartkatalog.geonorge.no/metadata?text=25833+WCS+h%C3%B8ydemodell): digital terrain model and digital surface model in UTM 33 (epsg:25833)
- [Nibio](https://nibio.no/tjenester/wms-tjenester/wms-tjenester-ar5): WMS (Web Map Services) Land cover maps
Available on [Gitlab](https://gitlab.met.no/pierreml/sitingclass) and can be installed from `remotes::install_git` with a token. I have to produce a token for each user that want to download my package. 


