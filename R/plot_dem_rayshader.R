#' Plot digital surface/elevation model in 3D with shadow rendering
#'
#' Use rayshader to plot surface elevation model near a weather station from
#' four cardinal angles
#'
#' @references \url{https://www.rayshader.com/}
#'
#' @param stn A SpatVector with station attributes from
#'        \code{"get_latlon_frost"}
#' @param dsm A SpatRaster of a digital surface model around the station
#' @param path A string path that defines where to save the plot, if NULL
#'        (default) the plot is printed on-screen and not saved
#'
#' @return A rendered image
#'
#' @examples
#' require(sf)
#'
#' # Load the station metadata and location
#' stn <- get_latlon_frost(stationid = 18700, paramid = 211)
#'
#' # Load a digital elevation model
#' dsm   <- download_dem_kartverket(stn, name = "dom", dx = 100, resx = 1)
#'
#' # Plot 3D DEM with rayshader
#' plot_dem_rayshader(stn, dsm, path = 'plot/dem3D')
#'
#' @importFrom rayshader raster_to_matrix sphere_shade ray_shade  render_camera
#' @importFrom rayshader add_shadow ambient_shade plot_3d render_label
#' @importFrom rayshader render_snapshot render_highquality render_depth
#' @importFrom stats setNames
#'
#' @export

plot_dem_rayshader <- function(stn = NULL,
                               dsm = NULL,
                               path = NULL) {

  # Extract station name, latlon and level
  stn.name    <- str_to_title(stn$station.name)

  # Convert DEM to a matrix:
  elmat <- rayshader::raster_to_matrix(dsm)

  # Compute rayshader's built-in textures and plot
  elmat %>%
    rayshader::sphere_shade(zscale = 10, texture = "imhof1") %>%
    rayshader::add_shadow(rayshader::ray_shade(elmat, zscale = 3), 0.5) %>%
    rayshader::add_shadow(rayshader::ambient_shade(elmat), 0) %>%
    rayshader::plot_3d(elmat,
                       zscale = 1,
                       fov = 70,
                       theta = -45,
                       zoom = 0.75,
                       phi = 45,
                       windowsize = c(1000, 800),
                       baseshape = "circle")

  # Add station label
  rayshader::render_label(elmat,
                          x = dim(elmat)[1] / 2,
                          y = dim(elmat)[2] / 2,
                          z = max(elmat, na.rm = TRUE) * 1.15,
                          text="",
                          linecolor = "white",
                          relativez = FALSE)
  # render_label(elmat, x = dim(elmat)[1]/2, y = dim(elmat)[2]/2,
  #            z = 200, zscale = 1,
  #            textcolor = "gray50", linecolor = "white", linewidth = 2,
  #            text = stn$id.stationid, relativez = FALSE, textsize = 2)

  # Print terrain and/or save a png
  Sys.sleep(0.2)

  # Save plot
  if (is.null(path)) {
    #rayshader::render_snapshot()

  } else {
    cardinal_array <- c("northward", "westward", "southward", "eastward")
    theta <- stats::setNames( seq(0, 359 , by = 90), cardinal_array)

    for (cardinal in cardinal_array) {
      dir.create(path, showWarnings = FALSE, recursive = TRUE)
      fname <- sprintf("%s/%i_terrain3D_%s.png",
                       path,
                       stn$id.stationid,
                       cardinal)
      title <- paste(stn.name, "-", cardinal)

      rayshader::render_camera(theta = theta[cardinal])
      # rayshader::render_snapshot(fname, title_text = title,
      #                            instant_capture=F, width = 2100,
      #                            height = 2100, title_size = 140)
      # #render_highquality(fname, clear = TRUE, title_text = stn.name,
      #                   width = 400, height = 400)
    }
  }

  #Add a title and vignette effect.
  #render_camera(theta=0,zoom=0.4,phi=30)
  #render_depth(title_text = "Blindern", bokehshape = "circle",focallength = 200, aberration = 0.3,
  #            title_size = 20, title_color = "white", title_bar_color = "black", vignette = TRUE)
  #render_highquality(samples=256, line_radius = 1, text_size = 18, text_offset = c(0,12,0),
  #                   clamp_value=10, clear = TRUE)
}
