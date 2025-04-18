% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_station_horizon_sun.R
\name{plot_station_horizon_sun}
\alias{plot_station_horizon_sun}
\title{Plot horizon with sun positions for a given station location}
\usage{
plot_station_horizon_sun(stn = NULL, path = stn$path)
}
\arguments{
\item{stn}{A SpatVector with station attributes from
\code{"get_metadata_frost"}}

\item{path}{A directory path defining where will be saved the plots,
if path is NULL the plots are printed to the console}
}
\value{
Sun diagram with station metadata
}
\description{
Plot sun position and horizons from local terrain model, local surface model
and regional digital terrain model with infos from the weather station.
}
\examples{
# Load the station metadata and location
stn <- get_metadata_frost(stationid = 18700)

# Plot sun diagram and save
plot_station_horizon_sun(stn, path  = "plot/horizon")

}
\references{
\url{https://frost-beta.met.no/docs/codeexamples}
}
