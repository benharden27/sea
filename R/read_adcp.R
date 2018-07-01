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

  # find the first bin depth and bin size
  si <- stringr::str_which(r, "1st Bin")
  bin1 <- as.numeric(tail(stringr::str_split(r[si],"\t")[[1]],1))
  di <- stringr::str_which(r, "Bin Size")
  binsize <- as.numeric(tail(stringr::str_split(r[di],"\t")[[1]],1))

  # finds the header line index and return headerline
  hi <- stringr::str_which(r, "^Ens.*Eas")

  headers <- readr::read_tsv(adcp_file, skip = hi-1, n_max = 0)
  headers <- names(headers)
  headers <- stringr::str_extract_all(headers,"\\w")
  headers <- purrr::map(headers,stringr::str_flatten)
  headers <- unlist(headers)

  # read the table in from 3 lines past the header onwards
  a <- readr::read_tsv(adcp_file, skip = hi+3, col_names = FALSE, col_types = readr::cols(.default = readr::col_double()))
  names(a) <- headers

  # find the indexes of u, v, lon and lat from titles in headerline
  ui <- stringr::str_which(names(a),"Eas")
  vi <- stringr::str_which(names(a),"Nor")
  loni <- stringr::str_which(names(a),"FLon")
  lati <- stringr::str_which(names(a),"FLat")

  # extract u and v, lon and lat
  u <- as.matrix(a[ui])
  v <- as.matrix(a[vi])
  lon <- c(a[loni])[[1]]
  lat <- c(a[lati])[[1]]

  d <- seq(bin1,by = binsize, length.out = dim(u)[2])
  dttm <- lubridate::ymd_hms(paste(a$YR,a$MO,a$DA,a$HH,a$MM,a$SS), tz = "UTC")

  # ensure that u and v are output as numeric
  u <- apply(u, 2, as.numeric)
  v <- apply(v, 2, as.numeric)
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)

  return(list(u = u, v = v, dttm = dttm, lon = lon, lat = lat, d = d))

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
read_adcp_fold <- function(adcp_fold, stack = T) {

  pattern = "1-30"
  files30 <- list.files(adcp_fold, pattern = paste0("ADCP.*1-30\\.txt"))
  files60 <- list.files(adcp_fold, pattern = paste0("ADCP.*31-60\\.txt"))

  file_start30 <- stringr::str_extract(files30,".*_.*_.*_")
  file_start60 <- stringr::str_extract(files60,".*_.*_.*_")

  Y <- list(lon = NULL, lat = NULL, dttm = lubridate::ymd_hms(NULL), u = NULL, v= NULL)

  for (i in 1:length(files30)) {
    adcp_file30 <- file.path(adcp_fold, files30[i])
    X <- read_adcp(adcp_file30)
    if (stack & file_start30[1] %in% file_start60) {
      adcp_file60 <- file.path(adcp_fold, paste0(file_start30[i],"31-60.txt"))
      X2 <- read_adcp(adcp_file60)
      X$u <- cbind(X$u,X2$u)
      X$v <- cbind(X$v,X2$v)
      dd <- diff(X$d)[1]
      X$d <- append(X$d,X2$d-X2$d[1]+dd+tail(X$d,1))

    } else {
      X$u <- cbind(X$u,matrix(NA,dim(X$u)))
      X$v <- cbind(X$v,matrix(NA,dim(X$v)))
    }

    Y$lon <- append(Y$lon,X$lon)
    Y$lat <- append(Y$lat,X$lat)
    Y$dttm <- append(Y$dttm,X$dttm)
    Y$d <- append(Y$d,X$d)
    Y$u <- rbind(Y$u,X$u)
    Y$v <- rbind(Y$v,X$v)
  }

  return(Y)

}


#' Read ADCP file that is a winADCP output
#'
#' @param adcp_file
#'
#' @return
#' @export
#'
#' @examples
read_adcp_ens <- function(adcp_file) {
  adcp <- oce::read.adp(adcp_file)
  adcp@data$v[,,1] <- adcp@data$v[,,1] + adcp@data$speedMadeGoodEast
  adcp@data$v[,,2] <- adcp@data$v[,,2] + adcp@data$speedMadeGoodNorth

  lat <- rowMeans(cbind(adcp@data$firstLatitude,adcp@data$lastLatitude))
  lon <- rowMeans(cbind(adcp@data$firstLongitude,adcp@data$lastLongitude))
  dttm <- rowMeans(cbind(adcp@data$firstTime,adcp@data$lastTime))
  d <- adp@data$distance

  adcp <- list(u = adcp@data$v[ , , 1], v = adcp@data$v[ , , 2],
               dttm = dttm, lon = lon, lat = lat, d = d)
}
