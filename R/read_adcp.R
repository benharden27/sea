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
    iblank[is.na(iblank)] <- 1
    nd <- length(adcp@data$distance)
    for (i in 1:length(iblank)) {
      adcp@data$v[i,iblank[i]:nd,1] <- NA
      adcp@data$v[i,iblank[i]:nd,2] <- NA
    }

  }


  lat <- rowMeans(cbind(adcp@data$firstLatitude,adcp@data$lastLatitude))
  lon <- rowMeans(cbind(adcp@data$firstLongitude,adcp@data$lastLongitude))

  firstTime <- adcp@data$firstTime
  firstTime[as.numeric(firstTime-runmed(firstTime,11))< -10000] <- NA
  if(length(which(is.na(firstTime))) > 0) {
    ti <- 1:length(firstTime)
    goodi <- !is.na(firstTime)
    firstTime <- approx(ti[goodi],firstTime[goodi],ti)$y
    firstTime <- as.POSIXct(firstTime,origin = "1970-1-1",tz = "UTC")
  }

  lastTime <- adcp@data$lastTime
  lastTime[as.numeric(lastTime-runmed(lastTime,11))< -10000] <- NA
  if(length(which(is.na(lastTime))) > 0) {
    ti <- 1:length(lastTime)
    goodi <- !is.na(lastTime)
    lastTime <- approx(ti[goodi],lastTime[goodi],ti)$y
    lastTime <- as.POSIXct(lastTime,origin = "1970-1-1",tz = "UTC")
  }

  dttm <- as.POSIXct(rowMeans(cbind(firstTime,lastTime)),origin = "1970-01-01")
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
read_adcp_ens_fold <- function(adcp_fold, combine = TRUE) {

  files <- list.files(adcp_fold,"(.LTA)|(.STA)|(.ENS)|(.ENX)")

  adcp_in = NULL
  for (i in 1:length(files)) {
    file <- file.path(adcp_fold,files[i])
    adcp_in[[i]] <- read_adcp_ens(file)
  }

  if(combine) {
    # find the longest depth vector in the adcp options and assign to dvec
    maxd <- rep(NA,length(adcp_in))
    for (i in 1:length(adcp_in)) {
      maxd[i] <- length(adcp_in[[i]]$d)
    }
    ii <- which.max(maxd)
    dvec <- adcp_in[[ii]]$d

    # loop through adcp objects, interpolate and combine
    for (i in 1:length(adcp_in)) {

      if (i == 1) {
        if(i == ii) {
          adcp_out <- adcp_in[[i]]
        } else {
          adcp_out <- interp_adcp(adcp_in[[i]],dvec)
        }

      } else {

        if(i == ii) {
          adcp_add <- adcp_in[[i]]
        } else {
          adcp_add <- interp_adcp(adcp_in[[i]],dvec)
          if(is.null(adcp_add)) {
            next
          }
        }

        adcp_out$u <- rbind(adcp_out$u,adcp_add$u)
        adcp_out$v <- rbind(adcp_out$v,adcp_add$v)
        adcp_out$backscat <- rbind(adcp_out$backscat,adcp_add$backscat)
        adcp_out$quality <- rbind(adcp_out$quality,adcp_add$quality)
        adcp_out$percent <- rbind(adcp_out$percent,adcp_add$percent)
        adcp_out$lon <- append(adcp_out$lon,adcp_add$lon)
        adcp_out$lat <- append(adcp_out$lat,adcp_add$lat)
        adcp_out$dttm <- append(adcp_out$dttm,adcp_add$dttm)
      }
    }
    return(adcp_out)
  } else {
    return(adcp_in)
  }
}


#' Interpolate an adcp field to different depths
#'
#' @param adcp
#' @param dvec
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
interp_adcp <- function(adcp,dvec,vars = c("u","v","backscat","quality","percent")) {

  # Create empty output object
  output <- NULL

  lenv <- dim(adcp[["u"]])[1]
  if(is.null(lenv)) {
    return(NULL)
  }

  for (var in vars) {

    out <- array(NA,c(lenv,length(dvec)))
    for (i in 1:lenv) {
      goodi <- !is.na(adcp[[var]][i,])
      if(sum(goodi)<2) {
        next
      } else {
        if(abs(min(adcp$d[goodi])-min(dvec))<10) {
          rule = 2:1
        } else {
          rule <- 1
        }
        out[i, ] <- approx(adcp$d[goodi],adcp[[var]][i,goodi],dvec,rule=rule)$y
      }
    }
    output[[var]] <- out
  }

  output[["lon"]] <- adcp[["lon"]]
  output[["lat"]] <- adcp[["lat"]]
  output[["dttm"]] <- adcp[["dttm"]]
  output[["d"]] <- dvec

  return(output)


}
