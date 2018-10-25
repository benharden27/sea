#' Get a matrix of a paricular variable from a list of oce ctd objects
#'
#' @param ctd
#' @param var
#'
#' @return
#' @export
#'
#' @examples
get_ctd_var <- function(ctd,var="temperature") {
  s <- make_section(ctd)
  s <- oce::sectionGrid(s)
  p <- unique(s[['pressure']])
  v <- array(NA, dim = c(length(ctd), length(p)))
  for (i in 1:length(ctd)) {
    v[i, ] <- s[['station']][[i]][[var]]
  }

}


#' Find index of nearest value in vector
#'
#' @param vec
#' @param vals
#'
#' @return
#' @export
#'
#' @examples
find_near <- function(vec,vals) {

  locs <- rep(NA,length(vals))
  for (i in 1:length(vals)) {
    if(!is.na(vals[i])) {
      locs[i] <- which.min(abs(vec-vals[i]))
    } else {
      locs[i] <- NA
    }
  }

  return(locs)

}


#' Repeat each element of a vector a set amount of times
#'
#' @param x
#' @param times
#'
#' @return
#' @export
#'
#' @examples
rep_each <- function(x,times) {
  unlist(purrr::map(x,rep,times))
}

#' Check if variable exists in data frame
#'
#' Assigns the variable to "val" if found, otherwise stops with error
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
