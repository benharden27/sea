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
  if(is_null(df) & is_null(lonlim) & is_null(latlim)) {
    lonlim = c(270,310)
    latlim = c(30,50)
  }

  # if there is an input data-frame
  if(!is_null(df)) {

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
    if(is_null(lonlim))
      lonlim <- set_ll_lim(df$lon)

    # Set latitude limits if not prescribed
    if(is.null(latlim))
      latlim <- set_ll_lim(df$lat)
  }

  # subset coastline data (TODO: need to ensure that 5 degs is a good selection for buffer)
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
make_dots <- function(df, data_source = "elg", var = "temp", step = 1, size = 2,
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

  if(!is_null(ran_val)) {
    df$val[df$val<ran_val[1]] <- ran_val[1]
    df$val[df$val>ran_val[2]] <- ran_val[2]
  }

  if(!is_null(ran_qua)) {
    quant <- quantile(df$val,ran_qua,na.rm=T)
    df$val[df$val<quant[1]] <- quant[1]
    df$val[df$val>quant[2]] <- quant[2]
  }

  # set up th range of values to be potted
  ran <- seq(1,nrow(df),step)

  # return a geom_points structure from data
  out <- geom_point(aes(x = lon, y = lat, color = val), data=df[ran, ],
                    pch = 21, size = size)


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
make_vectors <- function(df, data_source = "elg", field = "wind", step = 60) {

  # select the data source from sea structure
  if(is_sea_struct(df))
    df <- select_data(df,data_source)

  # add a function to fix the antimeridion cross
  df <- format_lon(df)

  if(field=="wind") {
    uv <- wswd_to_uv(df$wind_sp,df$wind_dir)
    df <- mutate(df,u = uv$u, v = uv$v)
  }

  vec <- make_vector_lonlat(df$lon,df$lat,df$u,df$v)
  df <- mutate(df,lone = vec$lone, late = vec$late)

  # set up th range of values to be potted
  ran <- seq(1,nrow(df),step)

  out <- geom_segment(aes(x = lon, y = lat, xend = lone, yend = late), data = df[ran, ])

}

