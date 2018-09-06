#' Convert kml station locations to output coordinates
#'
#' @param kml_file
#' @param file_out
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
kml_to_delim <- function(kml_file, file_out = NULL, ...) {

  kml_coord <- maptools::getKMLcoordinates(kml_file)
  kml_data <- tibble::as.tibble(t(matrix(unlist(kml_coord),3))[,1:2])

  lon_format <- lat_format <- rep(NA,dim(kml_data)[1])

  lon_gt0 <- kml_data[[1]]>0
  lat_gt0 <- kml_data[[2]]>0

  lon_format[!lon_gt0] <- paste0(format(abs(kml_data[[1]][!lon_gt0]), digits=5), "ºW")
  lon_format[lon_gt0] <- paste0(format(abs(kml_data[[1]][lon_gt0]), digits=5), "ºE")
  lat_format[!lat_gt0] <- paste0(format(abs(kml_data[[2]][!lat_gt0]), digits=4), "ºS")
  lat_format[lat_gt0] <- paste0(format(abs(kml_data[[2]][!lat_gt0]), digits=4), "ºN")

  station <- paste("Station", 1:dim(kml_data)[1])

  data_out <- tibble::tibble(station = station, lon = lon_format, lat = lat_format)

  if(is.null(file_out)) {
    file_out <- stringr::str_replace(kml_file, ".kml", ".txt")
  }

  readr::write_delim(data_out, file_out, ...)

}
