
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

