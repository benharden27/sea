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
plot_section <- function(X, adcp_var = "u", breaks = NULL, zlim = NULL, ylim = c(0,600), colormap = oce::oce.colorsTemperature()) {

  di <- find_near(X$d,ylim[1]):find_near(X$d,ylim[2])

  if(X$type == "adcp") {
    if(adcp_var == "u") {
      X$var <- X$var$u[ , di]
    } else {
      X$var <- X$var$v[ , di]
    }
  } else {
    X$var <- X$var[ ,di]
    X$d <- X$d[di]
  }

  if(!is.null(zlim)) {
    X$var[X$var < zlim[1]] <- zlim[1]
    X$var[X$var > zlim[2]] <- zlim[2]
  }

  # set up plotting ranges
  if(is.null(breaks)) {
    if (X$type == 'adcp') {
      abs_max <- max(abs(X$var),na.rm = T)
      breaks <- pretty(c(-abs_max,abs_max),n = 20)
    } else {
      breaks <- pretty(X$var,n = 20)
    }
  }

  # Create the colormaps
  if(X$type == "adcp") {
    var_cm <- oce::colormap(X$var, breaks = breaks, col = oce::oce.colorsVelocity)
  } else {
    var_cm <- oce::colormap(X$var, breaks = breaks, col = colormap)
  }


  # Plot temperature and add labels and profile lines
  oce::imagep(X$x, X$d, X$var, colormap = var_cm, flipy = TRUE, ylab = 'Depth [m]',
              filledContour = TRUE, missingColor = NULL,
              drawTriangles = T, ylim = ylim, zlim = zlim,
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
plot_section_map <- function(X, lonlim = NULL, latlim = NULL, factor = 0.15, ...) {

  if (!is.null(X$sec_lon)) {
    box <- generate_swath(X$sec_lon,X$sec_lat,X$width)
  } else {
    box = tibble::as.tibble(lon = NULL, lat = NULL)
  }

  if(is.null(lonlim))
    lonlim <- set_ll_lim(c(X$data_lon,box$lon), factor)

  if(is.null(latlim))
    latlim <- set_ll_lim(c(X$data_lat,box$lat), factor)

  data_pos = tibble::tibble(lon = X$data_lon, lat = X$data_lat)

  m <- make_base_map(lonlim = lonlim, latlim = latlim, ...) +
    ggplot2::geom_point(ggplot2::aes(lon, lat), data = data_pos)

  if (!is.null(X$sec_lon)) {
    sec_pos = tibble::tibble(lon = X$sec_lon, lat = X$sec_lat)
    m <- m +
      ggplot2::geom_polygon(ggplot2::aes(lon, lat), data = box, fill = 'grey30', alpha = .5) +
      ggplot2::geom_line(ggplot2::aes(lon, lat), data = sec_pos, color = "red")
  }

  m

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


  if(is.list(sec)) {
    if(is.null(select)) {
      select = 1:length(sec)
    }
    ctd <- sec
    sec <- make_section(sec,select = select)
  } else {
    if(is.null(select)) {
      select = 1:length(sec@metadata$stationId)
    }
      sec <- oce::subset(sec, stationId == select)
  }

  # grid the section data
  s <- oce::sectionGrid(sec)

  # create variables for number of stations and their lat/lons
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
      if(dist_to_section < width/2) {
        distctd[i] <- dist[which.min(oce::geodDist(sec_lon,sec_lat,lonctd[i],latctd[i]))]
      }
    }

    dist_vec <- distctd
  }

  p <- unique(s[['pressure']])
  d <- oce::swDepth(p, mean(latctd, na.rm = TRUE))

  if(is.null(dist_vec)) {
    dist <- oce::geodDist(sec, alongPath=T)
  } else {
    dist <- dist_vec
  }


  # Set up v arrays for plotting
  v <- array(NA, dim = c(length(which(!is.na(dist))), length(p)))
  for (i in which(!is.na(dist))) {
    v[i, ] <- s[['station']][[i]][[var]]
  }

  dist <- dist[!is.na(dist)]


  X <- list(data_lon = lonctd, data_lat = latctd, along_section = along_section,
            sec_lon = sec_lon, sec_lat = sec_lat,
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
      if(dist_to_section < width/2) {
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
      v[i, ] <- approx(prof$z,prof[[var]],d,rule=2:1)$y
    }
  }


  X = list(data_lon = lonloc, data_lat = latloc, along_section = along_section,
           sec_lon = sec_lon, sec_lat = sec_lat,
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
prep_section_adcp <- function(X, select = NULL, dist_vec = NULL,
                              along_section = F, sec_lon = NULL, sec_lat = NULL,
                              dx = 5, dz = 25, width = 10, grid = F, ...) {

  if(!is.null(select)) {
    X$lon <- X$lon[select]
    X$lat <- X$lat[select]
    X$dttm <- X$dttm[select]
    X$u <- X$u[select, ]
    X$v <- X$v[select, ]
  }

  if (along_section == T) {

    if(is.null(sec_lon))
      sec_lon <- X$lon[c(1,length(X$lon))]

    if(is.null(sec_lat))
      sec_lat <- X$lat[c(1,length(X$lat))]

    tot_dist <- oce::geodDist(sec_lon,sec_lat,alongPath = T)
    n <- ceiling(diff(range(tot_dist)) / dx * 5)
    sec_lon <- seq(sec_lon[1],sec_lon[2],length.out = n)
    sec_lat <- seq(sec_lat[1],sec_lat[2],length.out = n)
    dist <- oce::geodDist(sec_lon,sec_lat,alongPath = T)

    # find nearest section vector point to each CTD and assign that loaction to the CTD
    distloc <- rep(NA,length(X$lon))
    for (i in 1:length(X$lon)) {
      dist_to_section <- min(oce::geodDist(sec_lon,sec_lat,X$lon[i],X$lat[i]))
      if(dist_to_section < width/2) {
        distloc[i] <- dist[which.min(oce::geodDist(sec_lon,sec_lat,X$lon[i],X$lat[i]))]
      }
    }

    u <- v <- matrix(NA, n, dim(X$u)[2])
    for (i in 1:n) {
      ii <- distloc == dist[i]
      if (sum(distloc==dist[i],na.rm = T) == 0) {
        u[i, ] <- rep(NA, dim(X$u)[2])
        v[i, ] <- rep(NA, dim(X$u)[2])
      } else if (sum(distloc==dist[i],na.rm = T) == 1) {
        u[i, ] <- colMeans(X$u[ii, ], na.rm = T)
        v[i, ] <- colMeans(X$v[ii, ], na.rm = T)
      } else {
        u[i, ] <- colMeans(X$u[ii, ],na.rm = T)
        v[i, ] <- colMeans(X$v[ii, ],na.rm = T)
      }
    }

    sti <- rowSums(!is.na(u)) != 0

    u <- u[sti, ]
    v <- v[sti, ]
    dist_vec = dist[sti]

  } else {
    u <- X$u
    v <- X$v
  }

  if(is.null(dist_vec)) {
    dist <- oce::geodDist(X$lon, X$lat, alongPath = T)
  } else {
    dist <- dist_vec
  }

  depth <- seq(10,600, length.out = 60)

  if(grid == TRUE) {

    rows = dim(u)[1]
    cols = dim(u)[2]

    depth <-unlist(purrr::map(depth,rep,times=rows))
    dist <- rep(dist,cols)

    u <- grid_section(tibble::tibble(var=as.vector(u),dist=dist,depth=depth), ...)
    u <- u$z;
    v <- grid_section(tibble::tibble(var=as.vector(v),dist=dist,depth=depth), ...)
    dist <- v$x
    depth <- v$y
    v <- v$z


  }


  X <- list(data_lon = X$lon, data_lat = X$lat, along_section = along_section,
            sec_lon = sec_lon, sec_lat = sec_lat,
            dist_vec = dist_vec, width = width, dx = dx, dz = dz,
            d = depth, x = dist, var = list(u=u,v=v), name = "current", type = "adcp")

}


#' Grid section data onto a regular dist-depth grid
#'
#' @param df a data frame with three columns for var, dist, depth
#' @param xo distance vector
#' @param zo depth vector
#'
#' @return
#' @export
#'
#' @examples
grid_section <- function(df, xo = NULL, zo = NULL, ...) {

  # Set xo and zo to best guesses if not already perscribed
  n = length(unique(df$dist))
  if(is.null(xo))
    xo <- pretty(df$dist,n = n*2, n_min = n)

  if(is.null(zo))
    zo <- seq(5, 600, 50)


  ran <- !is.na(df$var)

  z <- akima::interp(df$dist[ran], df$depth[ran], df$var[ran],
                         xo = xo, yo = zo, linear = TRUE, ...)

}


