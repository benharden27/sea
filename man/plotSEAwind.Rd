% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SEAplots.R
\name{plotSEAwind}
\alias{plotSEAwind}
\title{Plot map of currents from onboard animometer}
\usage{
plotSEAwind(df, scale = 0.2, stp = 3, plotKEY = T, reg = "")
}
\arguments{
\item{df}{data frame from hourly data. Using readSEAxls() on <cruiseID>_hourlywork.xls}

\item{scale}{double to scale the vector arrows drawn}

\item{stp}{integer indicating the number of timesteps to skip between subsequent arrows.}

\item{plotKEY}{logical plot or don't plot}

\item{reg}{option to define a plotting region (calls reg2latlon).}
}
\description{
Plot map of currents from onboard animometer
}
\examples{
plotSEAwind()
}
