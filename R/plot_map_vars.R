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
plot_track <- function(df,data_source = "elg", ...) {

  make_base_map(df, ...) +
    make_track(df,data_source = data_source)

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
plot_flowthru <- function(df, var="temp", data_source = "elg", step = NULL,
                          colormap = oce::oce.colorsTemperature(),
                          ran_val = NULL, ran_qua = c(0.01,0.99),
                          title = stringr::str_to_title(var), type = "point", ...) {

  if(is.null(step)) {
    if(data_source == 'elg') {
      step = 60
    } else {
      step = 1
    }
  }

  make_base_map(df,title = title, ...) +
    make_points(df,data_source = data_source, var = var,
              step = step, ran_val = ran_val, ran_qua = ran_qua, type = type) +
    ggplot2::scale_color_gradientn(name = "", colors = colormap(100))

}

#' Plot a neuston tow dataset on a map
#'
#' @param df
#' @param var
#' @param data_source
#' @param step
#' @param colormap
#' @param ran_val
#' @param ran_qua
#' @param title
#' @param type
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_neuston <- function(df, var="biomass", data_source = "neuston", step = 1,
                         colormap = oce::oce.colorsTemperature(),
                         ran_val = NULL, ran_qua = c(0,1),
                         title = stringr::str_to_title(var), type = "bubble",...) {

  if(is_sea_struct(df)) {
    df1 <- select_data(df,data_source)
    make_base_map(df,title = title, ...) +
      make_track(df, color = "grey") +
      make_points(df1,var = var,data_source = data_source,
                  ran_qua = ran_qua, ran_val = ran_val, type = type)
  } else {
    plot_flowthru(df,var = var,data_source = data_source,
                  colormap = colormap, title = title, ran_val = ran_val,
                  ran_qua = ran_qua, type = type)

  }



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
plot_surf <- function(df, var = 'no3', ran_val = NULL, ran_qua = c(0,1), base_map = NULL, title = var, ...) {
  plot_flowthru(df,var = var,data_source = "surfsamp",colormap = oce::oce.colorsChlorophyll(), title = title)
}


#' Plot surface wind from an ELG or Hourly file
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
plot_wind <- function(df,data_source = "elg", step = 60, scale = 1,...) {

  make_base_map(df,...) +
    make_track(df,color = "grey") +
    make_vectors(df, data_source = data_source, step = step, scale = scale)


}


#' Plot surface from an ELG or Hourly file
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
plot_current <- function(df, step = 1, scale = 0.005,...) {

  make_base_map(df,...) +
    make_track(df, color = "grey", data_source = "adcp") +
    make_vectors(df, data_source = "adcp", step = step, scale = scale)


}

#' Make a bubble plot
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
plot_bubble <- function(df,var, ran_val = NULL, ran_qua = c(0,1), base_map = NULL, ...) {

  if(is.null(base_map))
    base_map <- make_base_map(df,...)

  if(check_antimerid(df))
    df$lon[df$lon<0] <- df$lon[df$lon<0] + 360

  df <- check_var(df,var)

  ii <- !is.na(df$val)

  df <- df[ii,]
  if(nrow(df)==0) {
    stop(paste('No values recorded for this field:',var))
  }

  base_map +
    ggplot2::geom_point(ggplot2::aes(x=lon,y=lat,size=val),data=df,pch=21,fill='white') +
    ggplot2::labs(color=var) +
    ggplot2::scale_radius(name = "")

}






