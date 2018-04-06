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
make_base_map <- function(df=NULL,lonlim=NULL,latlim=NULL,data_source = 'hourly',plot_bathy=F,high_res = F) {

  # choose which resolution of coastline
  if(high_res == T) {
    data(coastline_hr)
    coastline <- coastline_hr
  } else {
    data(coastline)
  }

  # choose default lon and lat if no df, lonlim or latlim are selected
  if(is.null(df) & is.null(lonlim) & is.null(latlim)) {
    lonlim = c(270,310)
    latlim = c(30,50)
  }

  # if there is an input data-frame
  if(!is.null(df)) {

    # if the input is an sea dataframe then extract the hourly (default) component
    # should add a contigency if no hourly data exists
    if(is_sea_struct(df)) {
      df <- select_data(df,data_source)
    }

    # if lon doesn't cross antimeridion, subtract 360 from
    if(!check_antimerid(df)) {
      coastline$long <- coastline$long - 360;
    }

    # format the longitude to correct for antimeridion now that coastline has been edited
    df <- format_lon(df)

    # load and subset bathymetry if requested
    if(plot_bathy)
      bathy <- extract_bathy(df)

    # Set longitude limits if not prescribed
    if(is.null(lonlim))
      lonlim <- set_ll_lim(df$lon)

    # Set latitude limits if not prescribed
    if(is.null(latlim))
      latlim <- set_ll_lim(df$lat)
  }

  # subset coastline data (TODO: need to ensure that 5 degs is a good selection for buffer)
  coastline <- subset(coastline,long > lonlim[1]-5 & long < lonlim[2]+5 & lat > latlim[1]-5 & lat < latlim[2]+5)

  # start ggplot of base map
  base_map <- ggplot2::ggplot()

  if(plot_bathy) {
    base_map <- base_map +
      ggplot2::geom_raster(ggplot2::aes(x,y,fill = z), data=bathy, interpolate = TRUE) +
      ggplot2::scale_fill_gradientn(colors = oce::oce.colorsGebco())
  }

  base_map <- base_map +
    ggplot2::geom_polygon(ggplot2::aes(x=long, y = lat, group = group),data=coastline) +
    ggplot2::coord_quickmap(xlim=lonlim, ylim=latlim, expand = F) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme_bw()

#
  xlabs <- pretty(df$lon)
  ylabs <- pretty(df$lat)

#   xlabs <- ggplot2::ggplot_build(base_map)$layout$panel_ranges[[1]]$x.major_source
#   ylabs <- ggplot2::ggplot_build(base_map)$layout$panel_ranges[[1]]$y.major_source
# #
  xlabs_old <- xlabs
  ylabs_old <- ylabs

  if (mean(xlabs, na.rm = T) < 0) {
    xlabs <- paste0(abs(xlabs),"ºW")
  } else {
    xlabs[xlabs_old>180] <- paste0(360-xlabs_old[xlabs_old>180],"ºW")
    xlabs[xlabs_old<=180] <- paste0(xlabs_old[xlabs_old<=180],"ºE")
  }
#
  if(mean(ylabs,na.rm=T) > 0) {
    ylabs <- paste0(ylabs,"ºN")
  } else {
    ylabs <- paste0(abs(ylabs),"ºS")
  }

#
  base_map <- base_map +
    ggplot2::scale_x_continuous(breaks = xlabs_old, labels = xlabs) +
    ggplot2::scale_y_continuous(breaks = ylabs_old, labels = ylabs)


  return(base_map)
}


#' Make a cruise track ggplot geom
#'
#' @param df
#' @param data_source
#'
#' @return
#' @export
#'
#' @examples
make_track <- function(df,data_source = "elg") {

  # select the data source from sea structure
  if(is_sea_struct(df))
    df <- select_data(df,data_source)

  # add a function to fix the antimeridion cross
  df <- format_lon(df)

  # return a geom_path object from the lon and lat data
  out <- geom_path(aes(lon,lat),data=df)

}


#' Generic generator of geom_points from data
#'
#' @param df
#' @param data_source
#' @param var
#' @param step
#' @param size
#' @param colormap
#'
#' @return
#' @export
#'
#' @examples
make_points <- function(df, var = "temp", data_source = "elg", step = 1, size = 2,
                      ran_val = NULL, ran_qua = c(0.01,0.99)) {

  # select the data source from sea structure
  if(is_sea_struct(df))
    df <- select_data(df,data_source)

  # add a function to fix the antimeridion cross
  df <- format_lon(df)

  # Assign variable if it exists
  if(is_var(df,var)) {
    val <- df[[var]]
    df <- mutate(df,val=val)
  } else {
    stop("Variable not found in data")
  }

  if(!is.null(ran_val)) {
    df$val[df$val<ran_val[1]] <- ran_val[1]
    df$val[df$val>ran_val[2]] <- ran_val[2]
  }

  if(!is.null(ran_qua)) {
    quant <- quantile(df$val,ran_qua,na.rm=T)
    df$val[df$val<quant[1]] <- quant[1]
    df$val[df$val>quant[2]] <- quant[2]
  }

  # set up th range of values to be potted
  ran <- seq(1,nrow(df),step)

  # return a geom_points structure from data
  out <- geom_point(aes(x = lon, y = lat, color = val), data=df[ran, ], size = size)


}


#' A geom of vector lines
#'
#' @param df
#' @param data_source
#' @param field
#' @param step
#'
#' @return
#' @export
#'
#' @examples
make_vectors <- function(df, data_source = "elg", field = "wind", step = 60, scale = 1) {

  # select the data source from sea structure
  if(is_sea_struct(df))
    df <- select_data(df,data_source)

  # add a function to fix the antimeridion cross
  df <- format_lon(df)

  if(field=="wind") {
    uv <- wswd_to_uv(df$wind_sp,df$wind_dir)
    df <- mutate(df,u = uv$u*scale, v = uv$v*scale)
  }

  vec <- make_vector_lonlat(df$lon,df$lat,df$u,df$v)
  df <- mutate(df,lone = vec$lone, late = vec$late)

  # set up th range of values to be potted
  ran <- seq(1,nrow(df),step)

  # TODO add arrows to end of line rather than dots
  out <- geom_segment(aes(x = lon, y = lat, xend = lone, yend = late), data = df[ran, ])

}

