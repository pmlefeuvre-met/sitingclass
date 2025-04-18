% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_landtype_horizon_class.R
\name{compute_landtype_horizon_class}
\alias{compute_landtype_horizon_class}
\title{Pipeline to compute siting class}
\usage{
compute_landtype_horizon_class(stn, f_plot = FALSE)
}
\arguments{
\item{stn}{A SpatVector with station attribute `stationid` from
\code{"get_latlon_frost"}}

\item{f_plot}{A boolean whether to plot and save figures}
}
\value{
None
}
\description{
The pipeline derives the exposure of a station based WMO/met.no siting
classification and includes four input functions:
\code{"download_dem_kartverket"}, \code{"compute_landtype"},
\code{"compute_landtype_distance"} and \code{"compute_horizon_max"}.
They provide the necessary input for \code{"compute_class_air_temperature"}.
}
\examples{
# Pipeline to compute input for deriving air_temperature siting class
# compute_landtype_horizon_class(stn)

}
