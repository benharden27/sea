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

  if(length(files30) == 0) {
    return(NULL)
  }

  file_start30 <- stringr::str_extract(files30,".*_.*_.*_")
  file_start60 <- stringr::str_extract(files60,".*_.*_.*_")

  Y <- list(lon = NULL, lat = NULL, dttm = lubridate::ymd_hms(NULL), u = NULL, v= NULL, d = NULL)

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
    Y$u <- rbind(Y$u,X$u)
    Y$v <- rbind(Y$v,X$v)
  }

  Y$d <- X$d

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

  if(hasName(adcp@data,"br")) {
    dblank = 25
    br <- rowMeans(adcp@data$br,na.rm = T) - dblank
    iblank <- sea::find_near(adcp@data$distance,br)
    nd <- length(adcp@data$distance)
    for (i in 1:length(iblank)) {
      adcp@data$v[i,iblank[i]:nd,1] <- NA
      adcp@data$v[i,iblank[i]:nd,2] <- NA
    }

  }

  lat <- rowMeans(cbind(adcp@data$firstLatitude,adcp@data$lastLatitude))
  lon <- rowMeans(cbind(adcp@data$firstLongitude,adcp@data$lastLongitude))
  dttm <- as.POSIXct(rowMeans(cbind(adcp@data$firstTime,adcp@data$lastTime)),origin = "1970-01-01")
  d <- adcp@data$distance

  backscat <- rowMeans(adcp[["a","numeric"]],dims=2)
  quality <- rowMeans(adcp[["q","numeric"]],dims=2)
  percent <- adcp[["g","numeric"]][ , , 4]

  adcp <- list(lon = lon, lat = lat, dttm = dttm, d = d,
               u = adcp@data$v[ , , 1], v = adcp@data$v[ , , 2],
               backscat = backscat, quality = quality, percent = percent)
}


#' Read all ADCP Ensemble files in a folder and combine
#'
#' @param adcp_fold
#'
#' @return
#' @export
#'
#' @examples
read_adcp_ens_fold <- function(adcp_fold) {

  files <- list.files(adcp_fold,".LTA")

  for (i in 1:length(files)) {

    file <- file.path(adcp_fold,files[i])
    adcp_add <- read_adcp_ens(file)
    if (i == 1) {
      adcp <- adcp_add
      dvec <- adcp$d
    } else {
      if(!sum(adcp_add$d==dvec) == length(dvec)) {
        u <- as.vector(adcp_add$u)
        dttm <- rep(adcp_add$dttm,ncol(adcp_add$u))
        d <- unlist(purrr::map(adcp_add$d,rep,nrow(adcp_add$u)))
        isna <- !is.na(u)
        u <- u[isna]
        dttm <- dttm[isna]
        d <- d[isna]
        adcp_add$u <- akima::interp(dttm,
                                    d,
                                    u,
                                    adcp_add$dttm,
                                    dvec,
                                    duplicate = "strip")$z

        v <- as.vector(adcp_add$v)
        v <- v[isna]
        adcp_add$v <- akima::interp(dttm,
                                    d,
                                    v,
                                    adcp_add$dttm,
                                    dvec,
                                    duplicate = "strip")$z
        adcp_add$d <- dvec
      }
      adcp$u <- rbind(adcp$u,adcp_add$u)
      adcp$v <- rbind(adcp$v,adcp_add$v)
      adcp$lon <- append(adcp$lon,adcp_add$lon)
      adcp$lat <- append(adcp$lat,adcp_add$lat)
      adcp$dttm <- append(adcp$dttm,adcp_add$dttm)
    }
  }
  return(adcp)
}
