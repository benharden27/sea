# Functions for plotting CTD data

#' Plot a CTD section
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
plot_section <- function(sec, var = "temperature", var_breaks = NULL, dist_vec = NULL, ylim = c(0,600)) {

  s <- oce::sectionGrid(sec)

  nstation <- length(s[['station']])
  latctd <- s@metadata$latitude
  lonctd <- s@metadata$longitude

  p <- unique(s[['pressure']])
  d <- oce::swDepth(p, mean(latctd, na.rm = TRUE))
  di = c(find_near(d, ylim[1]):find_near(d,ylim[2]))

  # Set up v arrays for plotting
  v <- array(NA, dim = c(nstation, length(p)))
  for (i in 1:nstation) {
    v[i, ] <- s[['station']][[i]][[var]]
  }

  # cut data below ylims
  v <- v[ ,di]
  d <- d[di]

  if(is.null(dist_vec)) {
    dist <- oce::geodDist(sec, alongPath=T)
  } else {
    dist <- dist_vec
  }

  if (check_antimerid(data.frame(lon = lonctd))) {
    lonctd[lonctd<0] <- lonctd[lonctd<0]+360
  }

  # set up plotting ranges
  if(is.null(var_breaks)) {
    var_breaks <- pretty(v,n = 10)
  }

  # Create the colormaps
  var_cm <- oce::colormap(v, breaks = var_breaks, col = oce::oceColorsTemperature)

  # Plot temperature and add labels and profile lines
  imagep(dist, d, v, colormap = var_cm, flipy = TRUE, ylab = 'Depth [m]',
         filledContour = TRUE, zlab = var, missingColor = NULL,
         drawTriangles = T, ylim = ylim,
         zlabPosition = 'side')

  # Add lines and labels
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)], c(0, max(sec[['station']][[i]][['depth']], na.rm = TRUE)), col = 'gray')
    # if (i==1 | geodDist(lonctd[cur], latctd[cur], lonctd[i], latctd[i]) > distmin) {
      mtext(s@metadata$stationId[i], 3 , 0 , at = dist[i])
      # cur <- i
    # }
  }

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


#' Grid a bottle derived value
#'
#' @param df data frame read in using read_hydrocast
#' @param var
#'
#' @return
#' @export
#'
#' @examples
grid_hydro_section <- function(df, var = "chla", select = NULL,
                               xo = NULL, yo = NULL, along_path = T) {

  stations <- unique(df$station)

  if(is.null(select)) {
    select <- 1:length(stations)
  }

  sti <- df$station %in% stations[select]

  lonloc <- df$lon[sti]
  latloc <- df$lat[sti]

  if (along_path) {
    dist <- oce::geodDist(lonloc, latloc, alongPath = T)
  } else {
    dist <- oce::geodDist(lonloc, latloc, lonloc[1], latloc[1])
  }

  if(is.null(xo))
    xo <- seq(0, max(dist, na.rm = TRUE), 5)

  if(is.null(yo))
    yo <- seq(5, 600, 50)


  df <- dplyr::mutate(df[sti, ], dist = dist)

  ran <- !is.na(df[[var]])

  z <- akima::interp(df$dist[ran], df$z[ran], df[[var]][ran],
                       xo = xo, yo = yo, linear = TRUE)


}
