#' Format
#'
#' @param data
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
format_odv <- function(data,folder,...) {

  # Return error is data is not a sea structure
  if(!is_sea_struct(data)) {
    stop("Data is not a sea package structure")
  }

  # create folder if it doesn't exist
  if (!file.exists(folder)){
    dir.create(folder)
  }

  # select the fields
  fields <- c("hourly","surfsamp","neuston")

  for (field in fields) {
    subfolder <- file.path(folder,field)
    if (!file.exists(subfolder)){
      dir.create(subfolder)
    }

    file <- file.path(subfolder,paste0(field,".txt"))

    if(field == "hourly") {
      format_hourly_odv(data,file)
    }

    if(field == "surfsamp") {
      format_surfsamp_odv(data,file)
    }

    if(field == "neuston") {
      format_neuston_odv(data,file)
    }

  }


}


#' Format ADCP Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_adcp_odv <- function(data,file,cruiseID = NULL) {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  # Convert from sea structure to get the adcp part
  if(is_sea_struct(data)){
    data <- data$adcp
  }

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = unlist(purrr::map(1:dim(data$u)[1],rep,dim(data$u)[2])),
    Type = "C",
    `mon/day/yr` = data$dttm,
    `Lon (∞E)` = data$lon,
    `Lat (∞N)` = data$lat,
    `Bot. Depth [m]` = " ",
    `Depth [m]` = rep(data$d,dim(data$u)[1]),
    `Echo Amplitude [counts]` = 0,
    `East Component [mm/s]` = as.vector(data$u)/1000,
    `North Component [mm/s]` = as.vector(data$v/1000),
    `Magnitude [mm/s]` = as.vector(sqrt(data$u^2+data$v^2)/1000),
    `Direction [deg]` = 0,
    Ensemble = 0,
    `hh:mm` =  00:00
  )

  readr::write_tsv(odv_out,file)

  return(odv_out)

}


#' Format Hourly Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_hourly_odv <- function(data,file,cruiseID = NULL) {

# Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  # Convert from sea structure to get the hourly part
  if(is_sea_struct(data)){
    data <- data$hourly
  }

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = 1:dim(data)[1],
    Type = "C",
    `mon/day/yr` = format(data$dttm,"%m/%d/%Y"),
    `Lon (∞E)` = data$lon,
    `Lat (∞N)` = data$lat,
    `Bot. Depth [m]` = data$depth_bot,
    `Depth [m]` = 0,
    `Temperature [∞C]` = data$temp,
    `Salinity [psu]` = data$sal,
    `Fluorescence` = data$fluor,
    `hh:mm` = format(data$dttm,"%H:%M"),
    `Wind Speed [knots]` = data$wind_sp,
    `Wind Direction [deg]` = data$wind_dir,
    `Wind-E/W Comp. [m/s]` = 0,
    `Wind-N/S Comp. [m/s]` = 0,
    `CDOM 1 min Avg` = data$cdom_1min,
    `Xmiss 1 min Avg` = data$xmiss_1min
  )

  readr::write_tsv(odv_out,file)

  return(odv_out)

}

#' Format Neuston Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_neuston_odv <- function(data,file,cruiseID = NULL) {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  # Convert from sea structure to get the hourly part
  if(is_sea_struct(data)){
    data <- data$neuston
  }

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = 1:dim(data)[1],
    Type = "X",
    `mon/day/yr` = format(data$dttm_in,"%m/%d/%Y"),
    `hh:mm` = format(data$dttm_in,"%H:%M"),
    `Lon (∞E)` = data$lon,
    `Lat (∞N)` = data$lat,
    `Depth [m]` = 0
  )

  ii <- which(names(data) == "temp")

  odv_out <- dplyr::bind_cols(odv_out,data[9:dim(data)[2]])

  readr::write_tsv(odv_out,file)

  return(odv_out)

}

#' Format Surface Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_surfsamp_odv <- function(data,file,cruiseID = NULL) {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  # Convert from sea structure to get the hourly part
  if(is_sea_struct(data)){
    data <- data$surfsamp
  }

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = 1:dim(data)[1],
    Type = "X",
    `mon/day/yr` = format(data$dttm_local,"%m/%d/%Y"),
    `hh:mm` = format(data$dttm_local,"%H:%M"),
    `Lon (∞E)` = data$lon,
    `Lat (∞N)` = data$lat,
    `Depth [m]` = 0
  )

  ii <- which(names(data) == "temp")

  odv_out <- dplyr::bind_cols(odv_out,data[9:dim(data)[2]])

  readr::write_tsv(odv_out,file)

  return(odv_out)

}
