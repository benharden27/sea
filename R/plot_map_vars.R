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

  make_base_map(df,...) +
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
plot_flowthru <- function(df, var='temp', data_source = "elg", step = NULL,
                          colormap = oce::oce.colorsTemperature(),
                          ran_val = NULL, ran_qua = c(0.01,0.99), ...) {

  if(is.null(step)) {
    if(data_source == 'elg') {
      step = 60
    } else {
      step = 1
    }
  }

  make_base_map(df,...) +
    make_points(df,data_source = data_source, var = var,
              step = step, ran_val = ran_val, ran_qua = ran_qua) +
    scale_color_gradientn(colors = colormap(100))

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
plot_surf <- function(df, var = 'no3', ran_val = NULL, ran_qua = c(0,1), base_map = NULL, ...) {
  plot_flowthru(df,var = var,data_source = "surfsamp",colormap = oce::oce.colorsChlorophyll())
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
    geom_point(aes(x=lon,y=lat,size=val),data=df,pch=21,fill='white') +
    labs(color=var)

}






