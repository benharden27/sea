# Create datasets for SEA package

# Create Coastline
library(maps)
library(mapdata)
coastline <- ggplot2::map_data("world2")
devtools::use_data(coastline,overwrite = T)
coastline_hr <- ggplot2::map_data("world2Hires")
devtools::use_data(coastline_hr)


# Create bathymetry
NOAA_5min <- marmap::getNOAA.bathy(lon1 = 0, lon2 = 0, lat1 = -90, lat2 = 90, resolution = 5, antimeridian = T)
devtools::use_data(NOAA_5min)

NOAA_10min <- marmap::getNOAA.bathy(lon1 = 0, lon2 = 0, lat1 = -90, lat2 = 90, resolution = 10, antimeridian = T)
devtools::use_data(NOAA_10min)

NOAA_1deg <- marmap::getNOAA.bathy(lon1 = 0, lon2 = 0, lat1 = -90, lat2 = 90, resolution = 60, antimeridian = T)
devtools::use_data(NOAA_1deg)

# create and save example SEA datasets
filein <- "~/data/SEA/S275/Event60sec_002.elg"
S275_elg <- read_elg(filein)
devtools::use_data(S275_elg)

filein <- "~/data/SEA/C276B/Event60sec_097.elg"
C276B_elg <- read_elg(filein)
devtools::use_data(C276B_elg)

# create structured data files
S269 <- package_data("~/data/SEA/S269")
devtools::use_data(S269,overwrite = T)

S275 <- package_data("~/data/SEA/S275")
devtools::use_data(S275,overwrite = T)

C276B <- package_data("~/data/SEA/C276B",forceGPS='nav')
devtools::use_data(C276B,overwrite = T)


C277A <- package_data("~/data/SEA/C277A")
devtools::use_data(C277A,overwrite = T)
