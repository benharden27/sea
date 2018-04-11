# Functions for plotting CTD data

#' Plot a section
#'
#' @param sec section object made using make_section (or oce functions)
#' @param var variable name to plot
#' @param var_breaks breaks for plotting
#' @param dist_vec alternative distance vector for plotting
#' @param ylim depth range to plot
#'
#' @return
#' @export
#'
#' @examples
plot_section <- function(X, var = "temp", select = NULL, dist_vec = NULL,
                         var_breaks = NULL, ylim = c(0,600),
                         along_section = F, sec_lon = NULL, sec_lat = NULL,
                         dx = 5, dz = 25, width = 10) {

  if(class(X[[1]]) == "ctd" | class(X) == "section") {
    X <- prep_section_ctd(X, var = var, dist_vec = dist_vec, along_path = along_path)
  } else if (is.data.frame(df)) {
    X <- prep_section_hydro(X, var = var, select = NULL, xo = xo, yo = yo, along_path = along_path, dist_vec = dist_vec)
  } else if (class(X) == "list") {
    X <- prep_section_adcp(X, sec_lon = sec_lon ,sec_lat = sec_lat, dx = dx, width = width)
  }


  di <- find_near(X$d,ylim[1]):find_near(X$d,ylim[2])

  if(is.list(X$var)) {
    X$var <-  X$var$u[ ,di]
  } else {
    X$var <- X$var[ ,di]
    X$d <- X$d[di]
  }

  # set up plotting ranges
  if(is.null(var_breaks)) {
    var_breaks <- pretty(X$var,n = 20)
  }

  # Create the colormaps
  var_cm <- oce::colormap(X$var, breaks = var_breaks, col = oce::oceColorsTemperature)

  # Plot temperature and add labels and profile lines
  oce::imagep(X$x, X$d, X$var, colormap = var_cm, flipy = TRUE, ylab = 'Depth [m]',
              filledContour = TRUE, missingColor = NULL,
              drawTriangles = T, ylim = ylim,
              zlabPosition = 'side')

  # # Add lines and labels
  # cur <- 1
  # for (i in 1:length(dist)){
  #   lines(dist[c(i,i)], c(0, max(sec[['station']][[i]][['depth']], na.rm = TRUE)), col = 'gray')
  #   # if (i==1 | geodDist(lonctd[cur], latctd[cur], lonctd[i], latctd[i]) > distmin) {
  #     mtext(s@metadata$stationId[i], 3 , 0 , at = dist[i])
  #     # cur <- i
  #   # }
  # }

}

#' Plot a map of ctd section locations
#'
#' @param sec section object to plot
#' @param labels logical as to whether station IDs should be printed
#' @param factor scale factor passed to set_ll_lim for setting range of plot
#' @param ... additional arguments to pass to make_section_map
#'
#' @return
#' @export
#'
#' @examples
plot_section_map <- function(sec, labels = TRUE, factor = 0.15, ...) {

  latctd <- sec@metadata$latitude
  lonctd <- sec@metadata$longitude

  lonlim <- set_ll_lim(lonctd, factor)
  latlim <- set_ll_lim(latctd, factor)

  m <- make_base_map(lonlim = lonlim, latlim = latlim, ...) +
    ggplot2::geom_point(ggplot2::aes(lonctd, latctd), data = data.frame(lonctd,latctd))

  if(labels == TRUE) {
    label <- sec@metadata$stationId
    m + ggplot2::geom_text(ggplot2::aes(lonctd, latctd, label = label),
                           nudge_x = diff(lonlim)/30, hjust = "left")
  } else {
    m
  }

}




#' Prepare ctd objects to plot as a section
#'
#' @param sec ctd list or section object to make into section
#' @param var variable to plot
#'
#' @return
#' @export
#'
#' @examples
prep_section_ctd <- function(sec, var = "temperature", select = NULL, dist_vec = NULL,
                             along_section = F, sec_lon = NULL, sec_lat = NULL,
                             dx = 5, dz = 5, width = 10) {

  # if the input is a list of ctds, convert to an OCE section object
  if(is.list(sec))
    sec <- make_section(sec)

  # select ctds that you user wants to plot
  if(is.null(select))
    select <- 1:length(sec@metadata$stationId)

  s <- oce::subset(sec, select %in% stationId)

  # grid the section data
  s <- oce::sectionGrid(s)

  # create variables for number of stations and their lat/lons
  nstation <- length(s[['station']])
  latctd <- s@metadata$latitude
  lonctd <- s@metadata$longitude

  # If plotting along a section...
  if(along_section) {

    # if no section longitude provided then asign to first and last values
    if(is.null(sec_lon))
      sec_lon <- s@metadata$longitude[c(1,length(s@metadata$longitude))]

    if(is.null(sec_lat))
      sec_lat <- s@metadata$latitude[c(1,length(s@metadata$latitude))]

    # create section vector using the dx incriment along the section
    tot_dist <- oce::geodDist(sec_lon,sec_lat,alongPath = T)
    n <- ceiling(diff(range(tot_dist)) / dx * 5)
    sec_lon <- seq(sec_lon[1],sec_lon[2],length.out = n)
    sec_lat <- seq(sec_lat[1],sec_lat[2],length.out = n)
    dist <- oce::geodDist(sec_lon,sec_lat,alongPath = T)

    # find nearest section vector point to each CTD and assign that loaction to the CTD
    distctd <- rep(NA,length(lonctd))
    for (i in 1:length(latctd)) {
      dist_to_section <- min(oce::geodDist(sec_lon,sec_lat,lonctd[i],latctd[i]))
      if(dist_to_section < width) {
        distctd[i] <- dist[which.min(oce::geodDist(sec_lon,sec_lat,lonctd[i],latctd[i]))]
      }
    }

    # subset to just those that were selected along section
    s <- subset(s,!is.na(distctd))
    dist_vec <- distctd[!is.na(distctd)]

  }

  p <- unique(s[['pressure']])
  d <- oce::swDepth(p, mean(latctd, na.rm = TRUE))

  # Set up v arrays for plotting
  v <- array(NA, dim = c(nstation, length(p)))
  for (i in 1:nstation) {
    v[i, ] <- s[['station']][[i]][[var]]
  }

  if(is.null(dist_vec)) {
    dist <- oce::geodDist(sec, alongPath=T)
  } else {
    dist <- dist_vec
  }

  X <- list(data_lon = lonctd, data_lat = latctd, sec_lon = sec_lon, sec_lat = sec_lat,
            dist_vec = dist_vec, width = width, dx = dx, dz = dz,
            d = d, x = dist, var = v, name = var, type = "ctd")

}

#' Prepare bottle-derived data to plot
#'
#' @param df data frame read in using read_hydrocast
#' @param var
#'
#' @return
#' @export
#'
#' @examples
prep_section_hydro <- function(df, var = "chla", select = NULL, dist_vec = NULL,
                               along_section = F, sec_lon = NULL, sec_lat = NULL,
                               dx = 5, dz = 25, width = 10) {

  sta_i <- !duplicated(df$station)
  stations <- df$station[sta_i]
  lonloc <- df$lon[sta_i]
  latloc <- df$lat[sta_i]

  # subset data based on selection
  if(is.null(select))
    select <- 1:length(stations)

  sti <- stations %in% stations[select]

  stations <- stations[sti]
  lonloc <- lonloc[sti]
  latloc <- latloc[sti]

  if (along_section) {

    if (is.null(sec_lon))
      sec_lon <- lonloc[c(1,length(lonloc))]

    if (is.null(sec_lat))
      sec_lat <- latloc[c(1,length(latloc))]

    # create section vector using the dx incriment along the section
    # TODO should functionalize this process
    tot_dist <- oce::geodDist(sec_lon,sec_lat,alongPath = T)
    n <- ceiling(diff(range(tot_dist)) / dx *5)
    sec_lon <- seq(sec_lon[1],sec_lon[2],length.out = n)
    sec_lat <- seq(sec_lat[1],sec_lat[2],length.out = n)
    dist <- oce::geodDist(sec_lon,sec_lat,alongPath = T)

    # find nearest section vector point to each CTD and assign that loaction to the CTD
    distloc <- rep(NA,length(lonloc))
    for (i in 1:length(lonloc)) {
      dist_to_section <- min(oce::geodDist(sec_lon,sec_lat,lonloc[i],latloc[i]))
      if(dist_to_section < width) {
        distloc[i] <- dist[which.min(oce::geodDist(sec_lon,sec_lat,lonloc[i],latloc[i]))]
      }
    }

    di <- !is.na(distloc)
    df <- dplyr::filter(df, lon %in% lonloc[di] & lat %in% latloc[di])
    dist_vec <- distloc[di]
    sta_i <- !duplicated(df$station)
    stations <- df$station[sta_i]
  }


  if(is.null(dist_vec)) {
    dist <- oce::geodDist(lonloc, latloc, alongPath = T)
  } else {
    dist <- dist_vec
  }




  d <- seq(0,max(df$z,na.rm=T),dz)


  v <- array(NA, dim = c(length(stations), length(d)))
  for (i in 1:length(stations)) {
    prof <- dplyr::filter(df,station == stations[i])
    if(length(which(!is.na(prof[[var]]))) == 0) {
      v[i, ] <- rep(NA,length(d))
    } else {
      v[i, ] <- approx(prof$z,prof[[var]],d)$y
    }
  }

  # if(is.null(xo))
  #   xo <- pretty(dist,n = floor(length(stations)*2), n_min = length(stations))
  #
  # if(is.null(yo))
  #   yo <- seq(5, 600, 50)
  #
  # df <- dplyr::mutate(df[sti, ], dist = dist)
  #
  # ran <- !is.na(df[[var]])
  #
  # z <- akima::interp(df$dist[ran], df$z[ran], df[[var]][ran],
  #                      xo = xo, yo = yo, linear = TRUE)

  X = list(data_lon = lonloc, data_lat = latloc, sec_lon = sec_lon, sec_lat = sec_lat,
           dist_vec = dist_vec, width = width, dx = dx, dz = dz,
           d = d, x = dist, var = v, name = var, type = "hydro")
}



#' Prepare ADCP data for plotting section
#'
#' @param X
#' @param sec_lon two point vector c(starting lon, ending lon)
#' @param sec_lat two point vector c(starting lat, ending lat)
#' @param dx distance between interpolated points along section in km
#' @param width width around which to gather data in km
#'
#' @return
#' @export
#'
#' @examples
prep_section_adcp <- function(X, sec_lon = NULL, sec_lat = NULL, dx = 5, width = 10) {

  if(is.null(sec_lon)) {
    sec_lon <- X$lon
  }

  if(is.null(sec_lat))
    sec_lat <- X$lat

  tot_dist <- oce::geodDist(sec_lon,sec_lat,alongPath = T)
  n <- ceiling(diff(range(tot_dist)) / dx)
  dist <- seq(tot_dist[1],tail(tot_dist,1),length.out = n)

  lonsec <- approx(1:length(sec_lon), sec_lon, n = n)$y
  latsec <- approx(1:length(sec_lat), sec_lat, n = n)$y

  dist_to_sec <- sec_loc <- rep(NA,length(X$lon))
  for (i in 1:length(X$lon)) {
    d <- oce::geodDist(lonsec,latsec,X$lon[i],X$lat[i])
    sec_loc[i] <- which.min(d)
    dist_to_sec[i] <- d[sec_loc[i]]
  }

  sec_loc[dist_to_sec > width/2] <- NA

  u <- v <- matrix(NA, n, dim(X$u)[2])
  for (i in unique(sec_loc[!is.na(sec_loc)])) {
    u[i, ] <- colMeans(X$u[sec_loc==i, ],na.rm = T)
    v[i, ] <- colMeans(X$v[sec_loc==i, ],na.rm = T)
  }

  X <- list(lon = lonsec, lat = latsec, d = seq(10,600, length.out = 60), x = dist, var = list(u=u,v=v), type = "adcp")


}



