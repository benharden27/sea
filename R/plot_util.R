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
  bathy_lat_vec <- unlist(purrr::map(bathy_lat,rep,length(bathy_lon)))


  bathy <- tibble::tibble(x=bathy_lon_vec,y=bathy_lat_vec,z=bathy_vec)

  # ggplot() +
  #   geom_raster(aes(x,y,fill = z), data=bathy, interpolate = TRUE) +
  #   scale_fill_gradientn(colors = oce.colorsGebco())
}

extract_data <- function(df,data_source = 'elg') {

  if(is_sea_struct(df)) {
    df <- select_data(df,data_source)
  }


}

#' Check for existence of standard SEA dataframe organization
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
is_sea_struct <- function(df) {
  field_names <- names(df)

  check_names <- c('elg','hourly','surfsamp','neuston','hydro')

  return(sum(check_names %in% field_names) == length(check_names))

}


#' Select a particular data source from standard data frame
#'
#' @param df
#' @param data_source
#'
#' @return
#' @export
#'
#' @examples
select_data <- function(df,data_source = 'elg') {

  df_names <- names(df)
  if(data_source %in% df_names){
    if(!is.null(df[[data_source]])) {
      out <- df[[data_source]]
    } else {
      stop("No data of this source in data frame")
    }
  } else {
    stop("Data source not found in data frame")
  }

  return(out)

}


#' Convert lon to all pos it cross anti-merid
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
format_lon <- function(df) {
  if(check_antimerid(df)) {
    df$lon[df$lon<0] <- df$lon[df$lon<0] + 360
  }

  return(df)

}



#' check if variable is in data frame
#'
#' @param df
#' @param var
#'
#' @return
#' @export
#'
#' @examples
is_var <- function(df,var) {

  df_names <- names(df)

  var %in% df_names

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



#' Check to se if crusie track crosses Anti-meridion
#'
#' @param df tibble from and read_ source
#'
#' @return
#' @export
#'
#' @examples
check_antimerid <- function(df) {
  if(is.data.frame(df) | is.list(df)) {
    diff(range(df$lon,na.rm=T))>300
  } else {
    diff(range(df,na.rm=T))>300
  }
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
    quant <- quantile(val,ran_qua,na.rm=T)
    val[val<quant[1]] <- quant[1]
    val[val>quant[2]] <- quant[2]
  }

  return(val)
}


#' Wind speed and direction to U and V
#'
#' @param ws
#' @param wd
#'
#' @return
#' @export
#'
#' @examples
wswd_to_uv <- function(ws,wd) {

  v = -ws*cos(wd*pi/180)
  u = -ws*sin(wd*pi/180)

  out <- tibble::tibble(u = u, v = v)

}


#' Turns vectors into lon and lat moved
#'
#' @param lon
#' @param lat
#' @param u
#' @param v
#'
#' @return
#' @export
#'
#' @examples
make_vector_lonlat <- function(lon,lat,u,v) {

  scale = .1

  late <- lat + v*scale
  lone <- lon + u*scale/cos(lat*pi/180)

  out <- tibble::tibble(lon=lon,lat=lat,lone=lone,late=late)

}



#' Generate swath for plotting sections
#'
#' @param lon
#' @param lat
#' @param width
#'
#' @return
#' @export
#'
#' @examples
generate_swath <- function(lon,lat,width) {

  gradient <- function(x,y) {

    yo <- dy <- dx <- rep(NA,length(y))
    ii <- 2:(length(yo)-1)

    dy[1] <- diff(head(y,2))
    dy[length(yo)] <- diff(tail(y,2))
    dy[ii] <- (y[ii+1]-y[ii-1])/2

    dx[1] <- diff(head(x,2))
    dx[length(yo)] <- diff(tail(x,2))
    dx[ii] <- (x[ii+1]-x[ii-1])/2

    yo <- dy / dx

    out <- list(dx = dx, dy = dy, yo = yo)

  }

  # how much do you go in lon for 1km
  lon_scale = 1/oce::geodDist(lon,lat,lon+1,lat)

  # how much do you go in lat for 1km
  lat_scale = 1/oce::geodDist(lon,lat,lon,lat+1)

  # find angle of section at any point
  z <- gradient(lon,lat)
  dx <- z$dx * lon_scale * width / 2
  dy <- z$dy * lat_scale * width / 2

  # one side
  box <- cbind(lon + dy / lon_scale, lat - dx / lat_scale)
  box2 <- cbind(lon - dy / lon_scale, lat + dx / lat_scale)
  box2 <- box2[seq(nrow(box), 1, length.out = nrow(box)), ]
  box <- rbind(box,box2)
  box <- rbind(box,box[1, ])

  box <- as.data.frame(box)
  names(box) <- c("lon","lat")

  return(box)

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





#' Extract just the lat and lon of the CTD profiles in a CTD list object
#'
#' Mostly for use in plotting locations of CTDs on cruise map
#'
#' @param CTDs CTD list object
#' @param X previousy extracted locations from another data source (e.g. Neustron tows)
#' @keywords
#' @export
#' @examples
#' readCTDsll()
readCTDsll <- function(CTDs,X=NULL) {
  for (i in 1:length(CTDs)) {
    X$lon <- append(X$lon,CTDs[[i]]@metadata$longitude)
    X$lat <- append(X$lat,CTDs[[i]]@metadata$latitude)
    X$flag <- append(X$flag,1)
  }
  return(X)
}




#' Extract just the lat and lon of the neuston tows stored in a data frame
#'
#' Mostly for use in plotting locations of neustons on cruise map
#'
#' @param df data frame containing Neuston tow data
#' @param X previousy extracted locations from another data source (e.g. CTDs)
#' @keywords
#' @export
#' @examples
#' readbioll()
readbioll <- function(df,X=NULL) {
  X$lon <- c(X$lon,df$LonDEC)
  X$lat <- c(X$lat,df$LatDEC)
  X$flag <- c(X$flag,rep(2,length(X$lon)))
  return(X)
}

