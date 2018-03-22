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
  diff(range(df$lon,na.rm=T))>300
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

