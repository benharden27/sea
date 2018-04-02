# utilities to read ADCP data

#' Read data from an ADCP file
#'
#' @param adcp_file
#'
#' @return
#' @export
#'
#' @examples
read_adcp <- function(adcp_file) {

  # reads in the file as individual lines
  r <- readr::read_lines(adcp_file)

  # finds the header line index and return headerline
  hi <- str_which(r, "^Ens.*Eas")

  headers <- read_tsv(adcp_file, skip = hi-1, n_max = 0) %>%
    names() %>%
    str_extract_all("\\w") %>%
    map(str_flatten) %>%
    unlist()

  # read the table in from 3 lines past the header onwards
  a <- readr::read_tsv(adcp_file, skip = hi+3, col_names = FALSE, col_types = readr::cols(.default = readr::col_double()))
  names(a) <- headers

  # find the indexes of u, v, lon and lat from titles in headerline
  ui <- str_which(names(a),"Eas")
  vi <- str_which(names(a),"Nor")
  loni <- str_which(names(a),"FLon")
  lati <- str_which(names(a),"FLat")

  # extract u and v, lon and lat
  u <- as.matrix(a[ui])
  v <- as.matrix(a[vi])
  lon <- c(a[loni])[[1]]
  lat <- c(a[lati])[[1]]

  dttm <- lubridate::ymd_hms(paste(a$YR,a$MO,a$DA,a$HH,a$MM,a$SS), tz = "UTC")

  # ensure that u and v are output as numeric
  u <- apply(u, 2, as.numeric)
  v <- apply(v, 2, as.numeric)
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)

  return(list(dttm = dttm, u = u, v = v, lon = lon, lat = lat))

}


#' Read adcp files from a folder
#'
#' @param adcp_fold
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
read_adcp_fold <- function(adcp_fold, pattern = "1-30") {

  files <- list.files(adcp_fold, pattern = paste0("ADCP.*",pattern,"\\.txt"))

  Y <- list(lon = NULL, lat = NULL, dttm = lubridate::ymd_hms(NULL), u = NULL, v= NULL)
  for (i in 1:length(files)) {
    adcp_file <- file.path(adcp_fold, files[i])
    cat(adcp_file)
    X <- read_adcp(adcp_file)
    Y$lon <- append(Y$lon,X$lon)
    Y$lat <- append(Y$lat,X$lat)
    Y$dttm <- append(Y$dttm,X$dttm)
    Y$u <- rbind(Y$u,X$u)
    Y$v <- rbind(Y$v,X$v)
  }

  return(Y)

}
