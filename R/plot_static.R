#' Extract ETOPO5 bathymetry for cruise region
#'
#' @param df
#' @param lonran
#' @param latran
#'
#' @return
#' @export
#'
#' @examples
extract_bathy <- function(df, lonran=NULL, latran=NULL) {

  # dat <- marmap::getNOAA.bathy(lon1 = 0, lon2 = 0, lat1 = -90, lat2 = 90, resolution = 10, antimeridian = T)
  topo <- readr::read_rds("./data/NOAA_10min.rds")
  topo_lon <- as.numeric(attributes(topo)$dimnames[[1]])
  topo_lat <- as.numeric(attributes(topo)$dimnames[[2]])

  if(is.null(lonran) | is.null(latran)) {
    lonran = range(df$lon,na.rm=T)
    latran = range(df$lat,na.rm=T)
  }

  lonlim <- lonran + c(-5,5)
  latlim <- latran + c(-5,5)

  lat1 <- find_near(topo_lat,latlim[1])
  lat2 <- find_near(topo_lat,latlim[2])
  topo <- topo[ ,lat1:lat2]

  if(lonlim[2]<0)
    topo_lon <- topo_lon - 360

  lon1 <- find_near(topo_lon,lonlim[1])
  lon2 <- find_near(topo_lon,lonlim[2])
  topo <- topo[lon1:lon2, ]

}


#' Find index of nearest value in vector
#'
#' @param vec
#' @param val
#'
#' @return
#' @export
#'
#' @examples
find_near <- function(vec,val) {
  which.min(abs(vec-val))
}

#' Create good lon/lat limtis from vector
#'
#' @param ll lon or lat vector
#' @param fact fraction more than range
#'
#' @return
#' @export
#'
#' @examples
set_ll_lim <- function(ll,factor=0.15) {
  ll_lim <- range(ll,na.rm=T)+diff(range(ll,na.rm=T))*c(-factor,factor)
}


#' Make a base map object
#'
#' @param df
#' @param bathy
#' @param lonlim
#' @param latlim
#'
#' @return
#' @export
#'
#' @examples
make_base_map <- function(df,bathy=F,lonlim=NULL,latlim=NULL) {

  # Determine if the cruise track crosses the anti-meridion and ammend lon as neccessary
  path_cross <- check_antimerid(df)
  if(path_cross)
    df$lon[df$lon<0] <- df$lon[df$lon<0] + 360

  # load and subset bathymetry if requested
  if(bathy) {
    topo <- extract_bathy(df)
    breaks <- c(-10000,-7000,-5000,-4000,-3000,-2000,-1000,-500,-200,-100)
  }

  # Set longitude limits if not prescribed
  if(is.null(lonlim))
    lonlim <- set_ll_lim(df$lon)

  # Set latitude limits if not prescribed
  if(is.null(latlim))
    latlim <- set_ll_lim(df$lat)

  # load coastline data
  # retrieved using: coastdata <- map_data("world2Hires")
  coastdata <- read_rds("./data/coastline.rds")
  if(!path_cross)
    coastdata$long <- coastdata$long - 360;

  coastdata <- subset(coastdata,long > lonlim[1] & long < lonlim[2] & lat > latlim[1] & lat < latlim[2])

  base_map <- ggplot(coastdata) +
    geom_polygon(aes(x=long, y = lat, group = group)) +
    coord_quickmap(xlim=lonlim, ylim=latlim, expand = F) +
    theme_bw()


}

#
# whr <- map_data("worldHires")
# w2hr <- map_data("world2Hires")

#' Plots map of cruise track
#'
#' @param df data frame generated either by readSEAxls or readSEAelgthat contains GPS data
#' @param type choose what type of data is in df (can be "elg" or "hourly")
#' @param bathy logical for including background bathymetry (FIX: CURRENTLY STORED LOCALLY)
#' @param stations option to include the output from readbioll() or readCTDsll() for locations of stations
#' @param reg option to define a plotting region (calls reg2latlon).
#'
#' @export
#' @examples
#' plot_track()
plot_track <- function(df,base_map=NULL,...) {

  if(is.null(base_map))
    base_map <- make_base_map(df,...)

  base_map +
    geom_path(data=df,aes(x=lon,y=lat))
    # geom_point(data=df,aes(x=lon,y=lat,color=temp))

}
