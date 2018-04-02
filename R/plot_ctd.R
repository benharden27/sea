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

  p <- unique(s[['pressure']])
  d <- oce::swDepth(p, mean(s@metadata$latitude, na.rm = TRUE))

  # Set up v arrays for plotting
  np <- length(p)
  v <- array(NA, dim = c(nstation, np))
  lonctd <- latctd <- rep(NA, nstation)

  for (i in 1:nstation) {
    v[i, ] <- s[['station']][[i]][[var]]
    lonctd[i] <- s[["station"]][[i]]@metadata$longitude
    latctd[i] <- s[["station"]][[i]]@metadata$latitude
  }

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
  imagep(dist, p, v, colormap = var_cm, flipy = TRUE, ylab = 'Depth [m]',
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
