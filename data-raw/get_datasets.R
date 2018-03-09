# Create datasets for SEA package

# Create Coastline
library(maps)
library(mapdata)
coastline <- map_data("world2Hires")
use_data(coastline)


# Create bathymetry
NOAA_5min <- marmap::getNOAA.bathy(lon1 = 0, lon2 = 0, lat1 = -90, lat2 = 90, resolution = 5, antimeridian = T)
use_data(NOAA_5min)

NOAA_10min <- marmap::getNOAA.bathy(lon1 = 0, lon2 = 0, lat1 = -90, lat2 = 90, resolution = 10, antimeridian = T)
use_data(NOAA_10min)

NOAA_1deg <- marmap::getNOAA.bathy(lon1 = 0, lon2 = 0, lat1 = -90, lat2 = 90, resolution = 60, antimeridian = T)
use_data(NOAA_1deg)


