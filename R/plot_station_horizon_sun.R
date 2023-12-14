#' Plot horizon with sun positions for a given station location
#'
#' Fetch station metadata based on station number and parameter id from Frost v1.
#' The function gets station name, location and sensor details such as level, exposure and performance
#'
#' @references \url{https://frost-beta.met.no/docs/codeexamples}
#'
#' @param stn A SpatVector with station attributes from \code{"get_latlon_frost"}
#' @param dem A SpatRaster of a digital elevation/terrain model around the station, expected radius is 100 m
#' @param dsm A SpatRaster of a digital surface model around the station, expected radius is 100 m
#' @param demkm A SpatRaster of a digital elevation/terrain model around the station, expected radius is 20 km
#' @param path A directory path defining where will be saved the plots, if path is NULL the plots are printed to the console
#'
#' @return Sun diagram with station metadata
#'
#' @examples
#' # Load data
#' stn <- get_latlon_frost(stationid=18700)
#' dem   <- download_dem_kartverket(stationid,centre,name="dtm",dx=100,resx=1)
#' dsm   <- download_dem_kartverket(stationid,centre,name="dom",dx=100,resx=1)
#' demkm <- download_dem_kartverket(stationid,centre,name="dtm",dx=20e3,resx=20)
#' path <- sprintf("station_location_files/output/%i",stn$id.stationid)
#' # path <- 'plot/horizon'
#' # Plot sun diagram
#' plot_station_horizon_sun(stn, dem, dsm, demkm, path=path)
#'
#' @import stringr
#' @import sf
#' @import ggplot2
#' @import ggfun
#'
#' @export

plot_station_horizon_sun <- function(stn = NULL,
                                     dem = NULL,
                                     dsm = NULL,
                                     demkm = NULL,
                                     path = NULL){

  # Libraries
  require(stringr)
  require(sf)
  require(ggplot2)
  require(ggfun) # element_roundrect()

  # Extract station name, latlon and level
  tz <- Sys.timezone()
  stn.name    <- str_to_title(stn$station.name)
  stn.id      <- stn$id.stationid
  stn.wmoid   <- stn$station.alternateids.id
  stn.level   <- stn$id.level
  stn.centre  <- stn  %>% st_coordinates
  stn.latlon  <- stn %>% st_transform(4326) %>% st_coordinates
  stn.param   <- stn$id.parameterid
  stn.expos   <- stn$timeseries.quality.exposure.value
  stn.perf    <- stn$timeseries.quality.performance.value
  if(stn.expos == "unknown"){stn.expos <- NA}
  if(stn.perf  == "unknown"){stn.perf  <- NA}

  # Set cardinals
  cardinals <- data.frame(azimuth=c(0,90,180,260,360),
                          inclination=rep(70,5),
                          labels=c("North","East","South","West","North"))

  # Compute sun position from station location
  sun       <- compute_sun_position(stn)
  sun_hour  <- compute_sun_position(stn,f.hour=T)

  # Compute horizon view from location
  horizon_dem   <- compute_horizon(stn.centre,dem,level=stn.level,step=.01,f.plot.polygon=T)
  horizon_dsm   <- compute_horizon(stn.centre,dsm,level=stn.level,step=.01,f.plot.polygon=T)
  horizon_demkm <- compute_horizon(stn.centre,demkm,level=stn.level,step=.01,f.plot.polygon=T)
  horizon_max   <- data.frame(azimuth=horizon_dem[,1],
                              horizon_height=apply(cbind(horizon_dem[,2],
                                                         horizon_dsm[,2],
                                                         horizon_demkm[,2]),
                                                   1,max) )
  skyviewfactor <- compute_skyviewfactor(horizon_max)

  # Plot init
  g <- ggplot()

  # Plot background
  g <- g +
    geom_hline(yintercept = c(0,7,20),linewidth=0.2,color="coral") +
    geom_text(data=cardinals,mapping=aes(x=azimuth,y=inclination,label=labels))

  # Plot horizon polygon
  g <- g +
    geom_polygon(data=horizon_max,mapping=aes(x=azimuth,y=horizon_height),alpha=0.5,fill="gray")
  # + geom_polygon(data=horizon_dsm  ,mapping=aes(x=azimuth,y=horizon_height),alpha=0.5,fill="gray")

  # Plot horizon lines
  g <- g +
    geom_line(data=horizon_dsm,mapping=aes(x=azimuth,y=horizon_height),linetype="dashed",colour="gray") +
    geom_line(data=horizon_dem,mapping=aes(x=azimuth,y=horizon_height),colour="gray",alpha=0.7) +
    geom_line(data=horizon_demkm,mapping=aes(x=azimuth,y=horizon_height))

  # Plot sun position from the station location
  g <- g +
    geom_path(data=sun_hour, aes(x=azimuth, y=inclination, group=hour), linewidth=.2, color="coral") +
    geom_line(data=sun,      aes(x=azimuth, y=inclination, color=day)) +
    scale_color_viridis_d()

  # Set theme and legend
  g <- g +
    theme_minimal() +
    theme(legend.position = c(0.9, 0.65),
          legend.background = element_roundrect(fill = "white"),
          legend.key.size = unit(.75, "lines")) +
    labs(color=NULL)

  # Set axis breaks, limits and labels
  xmin <- 0
  xmax <- 360
  g <- g +
    scale_x_continuous(breaks = seq(xmin, xmax, by = 30), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 55 , by = 5 ), expand = c(0, 0)) +
    coord_cartesian(xlim=c(xmin,xmax),ylim=c(0,70),expand=T)

  # Add annotation with infos
  label <- sprintf("Norwegian Met. Off.\n")
  label <- sprintf("%slat: %02.2f - long: %02.2f - elev: %1.0f\n",
                   label,stn.latlon[1], stn.latlon[2],stn$elev)
  label <- sprintf("%sparamid: %i - exp.: %i - perf.: %i\n",
                   label, stn.param, stn.expos, stn.perf)
  label <- sprintf("%stime_zone: %s+1 - svf: %02.2f\n",label, tz, skyviewfactor)
  label <- sprintf("%sstation_id: %i - level: %i\n",label, stn.id, stn.level)
  label <- sprintf("%swmo_id: %s\n%s",label,stn.wmoid, stn.name)
  g <- g +
    annotate("label", x = 10, y = 50, size = 3,hjust = 0,
             label= label)

  # Save plot
  if(!is.null(path)){
    fname <- sprintf("%s/%i_sun_diagram_auto.png",path,stn$id.stationid)
    ggsave(fname,bg="white", width = 7, height = 7)
  }

  return(g)
}
