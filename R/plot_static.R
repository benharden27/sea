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
plot_flowthru <- function(df,var='temp',ran_val = NULL, ran_qua = c(0.01,0.99),
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
    scale_color_gradientn(colors = oce.colorsTemperature(100)) +
    labs(color=var)
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

  df$val <- fix_range(df$val,ran_val,ran_qua)

  base_map +
    geom_point(aes(x=lon,y=lat,fill=val),data=df, pch=21, size=5) +
    scale_color_gradientn(colors = oce::oce.colorsDensity(100)) +
    labs(color=var)
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






