#' Plot cruise track on leaflet map
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
plot_track_leaflet <- function(df) {
  m <- leaflet(df) %>%
    addTiles() %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    addPolylines(lng = ~lon, lat = ~lat)
  m
}



#' Plot the flow-through data on a leaflet map
#'
#' @param dfdata frame read from read_elg or read_hourly
#' @param type temp, sal or fluor currently
#' @param step gap between consecutive colored dots
#'
#' @return
#' @export
#'
#' @examples
plot_flowthrough <- function(df,type='temp',step = 60) {

  if(check_antimerid(df))
    df$lon[df$lon<0] <- df$lon[df$lon<0] + 360


  if(type == 'temp') {
    values <- df$temp
    palette <- oce.colorsTemperature(100)
  } else if (type == 'sal') {
    values <- df$sal
    palette <- oce.colorsSalinity(100)
  } else if (type == 'fluor') {
    values <- df$fluor
    palette <- oce.colorsChlorophyll(100)
  }

  ran <- quantile(values,c(0.01,0.99),na.rm=T)
  pal <- colorNumeric(palette,ran,na.color = NA)

  m <- leaflet(df) %>%
    addTiles() %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    # addProviderTiles(providers$OpenSeaMap) %>%
    addPolylines(lng = ~lon, lat = ~lat, color = 'black', weight = 2) %>%
    addCircles(lng= ~lon[seq(1,nrow(df),step)], lat = ~lat[seq(1,nrow(df),step)],
               color= pal(values)[seq(1,nrow(df),step)], radius = 3, fillOpacity = 1, opacity = 1) %>%
    addLegend(position="bottomright", pal = pal, values = ran, opacity = 1)

  m

}

