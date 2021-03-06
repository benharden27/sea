% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SEAplots.R
\name{plotSEAelg}
\alias{plotSEAelg}
\title{Plot flowthrough data stored in an ELG file}
\usage{
plotSEAelg(df, vars = c(1, 2, 3, 4), step = 60, reg = "",
  bathy = T, new_elg = F)
}
\arguments{
\item{df}{data frame produced by using readSEAelg().}

\item{vars}{options for which variables to plot}

\item{step}{gap between subsequent plotted points}

\item{reg}{option to define a plotting region (calls reg2latlon).}

\item{bathy}{logical for including background bathymetry (FIX: CURRENTLY STORED LOCALLY)}
}
\description{
Takes data loaded in from an ELG file and plots a map of a number of flowthrough variables along the cruise track
}
\details{
Notes: vars can be:

1 - Temperature

2 - Salinity

3 - Fluoroescence

4 - CDOM Fluoroescence
}
\examples{
plotSEAelg()
}
