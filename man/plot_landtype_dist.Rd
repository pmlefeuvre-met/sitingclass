% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_landtype_dist.R
\name{plot_landtype_dist}
\alias{plot_landtype_dist}
\title{Plot area-landtype distribution}
\usage{
plot_landtype_dist(stn = NULL, land = NULL)
}
\arguments{
\item{stn}{A SpatVector with station attributes from
\code{"get_latlon_frost"}}

\item{land}{A data.frame with land type distribution from
\code{"compute_landtype_distance"}}
}
\value{
A figure
}
\description{
Plot distribution of landtype area with distance from a weather station to
asses the surrounding environment and potnetial impact on the measurements
}
\examples{
# Example of input to plot
# plot_landtype_dist(stn, landtype_dist)

}
