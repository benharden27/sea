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

  data("NOAA_5min")
  bathy <- NOAA_10min
  bathy_lon <- as.numeric(attributes(bathy)$dimnames[[1]])
  bathy_lat <- as.numeric(attributes(bathy)$dimnames[[2]])

  if(is.null(lonran) | is.null(latran)) {
    lonran = range(df$lon,na.rm=T)
    latran = range(df$lat,na.rm=T)
  }

  lonlim <- lonran + c(-5,5)
  latlim <- latran + c(-5,5)

  lat1 <- find_near(bathy_lat,latlim[1])
  lat2 <- find_near(bathy_lat,latlim[2])
  bathy <- bathy[ ,lat1:lat2]
  bathy_lat <- bathy_lat[lat1:lat2]

  if(lonlim[2]<0)
    bathy_lon <- bathy_lon - 360

  lon1 <- find_near(bathy_lon,lonlim[1])
  lon2 <- find_near(bathy_lon,lonlim[2])
  bathy <- bathy[lon1:lon2, ]
  bathy_lon <- bathy_lon[lon1:lon2]

  bathy_vec <- as.vector(bathy)
  bathy_vec[bathy_vec>0] <- 0
  bathy_lon_vec <- rep(bathy_lon,length(bathy_lat))
  bathy_lat_vec <- unlist(map(bathy_lat,rep,length(bathy_lon)))


  bathy <- tibble(x=bathy_lon_vec,y=bathy_lat_vec,z=bathy_vec)

  # ggplot() +
  #   geom_raster(aes(x,y,fill = z), data=bathy, interpolate = TRUE) +
  #   scale_fill_gradientn(colors = oce.colorsGebco())
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
make_base_map <- function(df,plot_bathy=F,lonlim=NULL,latlim=NULL) {

  data(coastline)
  path_cross <- check_antimerid(df)

  if(path_cross) {
    df$lon[df$lon<0] <- df$lon[df$lon<0] + 360
  } else {
    coastline$long <- coastline$long - 360;
  }

  # load and subset bathymetry if requested
  if(plot_bathy) {
    bathy <- extract_bathy(df)
  }

  # Set longitude limits if not prescribed
  if(is.null(lonlim))
    lonlim <- set_ll_lim(df$lon)

  # Set latitude limits if not prescribed
  if(is.null(latlim))
    latlim <- set_ll_lim(df$lat)

  # load coastline data
  # retrieved using: coastdata <- map_data("world2Hires")


  coastline <- subset(coastline,long > lonlim[1] & long < lonlim[2] & lat > latlim[1] & lat < latlim[2])

  base_map <- ggplot()

  if(plot_bathy) {
    base_map <- base_map +
      geom_raster(aes(x,y,fill = z), data=bathy, interpolate = TRUE) +
      scale_fill_gradientn(colors = oce.colorsGebco())
  }

  base_map <- base_map +
    geom_polygon(aes(x=long, y = lat, group = group),data=coastline) +
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

  if(check_antimerid(df))
    df$lon[df$lon<0] <- df$lon[df$lon<0] + 360

  base_map +
    geom_path(aes(lon,lat),data=df)
    # geom_point(data=df,aes(x=lon,y=lat,color=temp))
}


#' Plot flow through data
#'
#' @param df
#' @param type
#' @param base_map
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_surfvals <- function(df,type='temp',step=60,base_map=NULL,...) {

  if(is.null(base_map))
    base_map <- make_base_map(df,...)

  if(check_antimerid(df))
    df$lon[df$lon<0] <- df$lon[df$lon<0] + 360

  val <- df[[type]]
  if(is.null(val)) {
    stop('Data type not found in data')
  } else {
    df <- dplyr::mutate(df,val = val)
  }

  ran <- seq(1,nrow(df),step)

  base_map +
    geom_path(aes(x=lon,y=lat),data=df) +
    geom_point(aes(x=lon,y=lat,color=val),data=df) +
    scale_color_gradientn(colors = oce.colorsTemperature(100))
}



#' Set a colormap
#'
#' @param values
#' @param palette
#' @param clim
#' @param method
#'
#' @return
#' @export
#'
#' @examples
set_colormap <- function(values,palette,clim=NULL,method='quantile') {

  if (is.null(clim)) {
    ran <- quantile(values,c(0.01,0.99),na.rm=T)
  } else if (lenght(clim) == 2 & is.numeric(clim)) {
    if (method == 'quantile') {
      ran <- quantile(values,clim,na.rm=T)
    } else {
      ran <- clim
    }
  } else {
    stop('clim not a valid vector of two numbers')
  }

  pal <- leaflet::colorNumeric(palette,ran,na.color = NA)

}
