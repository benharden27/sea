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
make_base_map <- function(df=NULL,lonlim=NULL,latlim=NULL,plot_bathy=F,high_res = F) {

  if(high_res == T) {
    data(coastline_hr)
    coastline <- coastline_hr
  } else {
    data(coastline)
  }

  if(is_null(df) & is_null(lonlim) & is_null(latlim)) {
    lonlim = c(270,310)
    latlim = c(30,50)
  }

  if(!is_null(df)) {
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
    if(is_null(lonlim))
      lonlim <- set_ll_lim(df$lon)

    # Set latitude limits if not prescribed
    if(is.null(latlim))
      latlim <- set_ll_lim(df$lat)
  }

  # load coastline data
  coastline <- subset(coastline,long > lonlim[1]-5 & long < lonlim[2]+5 & lat > latlim[1]-5 & lat < latlim[2]+5)

  # start ggplot of base map
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

