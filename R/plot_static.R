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
make_base_map <- function(df,plot_bathy=F,lonlim=NULL,latlim=NULL,high_res = F) {

  if(high_res == T) {
    coastline <- data(coastline_hr)
  } else {
    coastline <- data(coastline)
  }

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
plot_surfvals <- function(df,var='temp',ran_val = NULL, ran_qua = c(0.01,0.99),
                          step = 60, base_map = NULL,...) {

  if(is.null(base_map))
    base_map <- make_base_map(df,...)

  if(check_antimerid(df))
    df$lon[df$lon<0] <- df$lon[df$lon<0] + 360

  val <- df[[var]]
  if(is.null(val)) {
    stop('Data type not found in data')
  } else {
    df <- dplyr::mutate(df,val = val)
  }

  ran <- seq(1,nrow(df),step)

  if(!is_null(ran_val)) {
    df$val[df$val<ran_val[1]] <- ran_val[1]
    df$val[df$val>ran_val[2]] <- ran_val[2]
  }

  if(!is_null(ran_qua)) {
    quant <- quantile(df$val,ran_qua,na.rm=T)
    df$val[df$val<quant[1]] <- quant[1]
    df$val[df$val>quant[2]] <- quant[2]
  }

  base_map +
    geom_path(aes(x=lon,y=lat),data=df) +
    geom_point(aes(x=lon,y=lat,color=val),data=df[ran,]) +
    scale_color_gradientn(colors = oce.colorsTemperature(100))
}


#' Plot surface station data
#'
#' @param df
#' @param var
#' @param ran_val
#' @param ran_qua
#' @param base_map
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_surf <- function(df, var = 'no3', ran_val = NULL, ran_qua = c(0.01,0.99), base_map = NULL, ...) {

  if(is.null(base_map))
    base_map <- make_base_map(df,...)

  if(check_antimerid(df))
    df$lon[df$lon<0] <- df$lon[df$lon<0] + 360

  df <- check_var(df,var)

  ii <- !is.na(df$val)

  df <- df[ii,]
  if(nrow(df)==0) {
    stop('No values recorded for this field')
  }

  df$val <- fix_range(df$val,ran_val,ran_qua)

  base_map +
    geom_point(aes(x=lon,y=lat,color=val),data=df) +
    scale_color_gradientn(colors = oce.colorsDensity(100))
}



#' Check if variable exists in data frame
#'
#' @param df
#' @param var
#'
#' @return
#' @export
#'
#' @examples
check_var <- function(df,var) {

  val <- df[[var]]
  if(is.null(val)) {
    stop('Data type not found in data')
  } else {
    df <- dplyr::mutate(df,val = val)
  }

  return(df)
}


#' Fix color region
#'
#' @param val
#' @param ran_val
#' @param ran_qua
#'
#' @return
#' @export
#'
#' @examples
fix_range <- function(val,ran_val=NULL,ran_qua = c(0.01,0.99)) {

  if(!is_null(ran_val)) {
    val[val<ran_val[1]] <- ran_val[1]
    val[val>ran_val[2]] <- ran_val[2]
  } else if (!is_null(ran_qua)) {
    quant <- quantile(df[[var]],ran_qua,na.rm=T)
    val[val<quant[1]] <- quant[1]
    val[val>quant[2]] <- quant[2]
  }
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
