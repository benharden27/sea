
#' Find index of nearest value in vector
#'
#' @param vec
#' @param val
#'
#' @return
#' @export
#'
#' @examples
find_near <- function(vec,val) {
  which.min(abs(vec-val))
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
