
#
# whr <- map_data("worldHires")
# w2hr <- map_data("world2Hires")

#' Plots map of cruise track
#'
#' @param df data frame generated either by readSEAxls or readSEAelgthat contains GPS data
#' @param type choose what type of data is in df (can be "elg" or "hourly")
#' @param bathy logical for including background bathymetry (FIX: CURRENTLY STORED LOCALLY)
#' @param stations option to include the output from readbioll() or readCTDsll() for locations of stations
#' @param reg option to define a plotting region (calls reg2latlon).
#'
#' @export
#' @examples
#' plotSEAct()
plot_track <- function(df,bathy=T,stations=NULL,reg=NULL,legend = T) {


  if(bathy) {
    # Read in Bathymetry
    load("~/Documents/projects/SEA/data/etopo5")
    lonran = range(lon,na.rm=T)
    latran = range(lat,na.rm=T)
    lonlim <- lonran + c(-10,10)
    latlim <- latran + c(-10,10)
    topo <- oce::subset(topo, latlim[1] < latitude & latitude < latlim[2])
    if(lonran[2]>180) {
      topo@data$longitude <- topo@data$longitude + 360
    }
    topo <- oce::subset(topo, lonlim[1] < longitude & longitude < lonlim[2])
    if(lonran[2]<180) {
      topo@data$longitude <- topo@data$longitude + 360
    }
    breaks <- c(-10000,-7000,-5000,-4000,-3000,-2000,-1000,-500,-200,-100,0)
  }

  if(nchar(reg)>0) {
    Y <- reg2latlon(reg)
    latlim <- Y$latlim
    lonlim <- Y$lonlim
  } else {
    latlim <- range(lat,na.rm=T)+diff(range(lat,na.rm=T))*c(-.15,.15)
    lonlim <- range(lon,na.rm=T)+diff(range(lon,na.rm=T))*c(-.15,.15)
  }


  if(coastline=="regular"){
    data("coastlineWorld")
    coastdata <- coastlineWorld
  } else {
    data("coastlineWorldFine")
    coastdata <- coastlineWorldFine
  }
  par(mar=c(3, 3, 1, 1),mgp=getOption("oceMgp"))
  mapPlot(coastdata,proj="+proj=merc +lon_0=180",
          latitudelim = latlim,
          longitudelim = lonlim,
          col='gray',axes=FALSE, grid=FALSE)
  latlabels <- seq(-90, 0, 1)
  lonlabels <- c(seq(0, 179, 1), seq(-180, 0, 1))
  if(bathy) {
    mapImage(topo,filledContour=TRUE, col=oce.colorsGebco, breaks=breaks)
  }
  mapLines(lon,lat,col=1)
  if(bathy) {
    mapPolygon(coastdata,col='gray')
  }
  mapGrid(longitude=lonlabels, latitude=latlabels)
  mapAxis(longitude=lonlabels, latitude=latlabels)
  if(!is.null(stations)) {
    mapPoints(stations$lon[stations$flag==1],stations$lat[stations$flag==1],pch=2,col=2)
    mapPoints(stations$lon[stations$flag==2],stations$lat[stations$flag==2],pch=6,col=1)
    xloc = par('xaxp')[1]#+diff(par('xaxp')[1:2])/20
    yloc = par('yaxp')[2]#-diff(par('yaxp')[1:2])/20
    if(legend) {
      legend(xloc,yloc,c('CTD','Neuston'),pch=c(2,6),col=c(2,1))
    }
  }

}


# add_na_gaps <- function(a) {
#
#   a <- as_tibble(a)
#   blank_row = list(long=NA,lat=NA,group=NA,order=NA,region=NA,subregion=NA)
#   b <- a
#   ii <- which(diff(a$group)>0)
#   for (i in ii) {
#     a <- add_row(a,long=NA,.after = i)
#   }
#
# }

# make_map <- function()
#
#   f <- function(df,group) {
#     polygon(df$lon[df$group==group],df$lat[df$group==group],type="l")
#   }
#
# groups = unique(a$group)
# for (i in 1:length(groups)) {
#   if (i == 1) {
#     plot(df$lon[df$group==group],df$lat[df$group==group],type="l")
#   } else {
#     polygon(df$lon[df$group==group],df$lat[df$group==group],type="l")
#   }
# }


#' Plot a cruise track from an sea data frame containing lon and lat data
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
plot_cruise_track_gg <- function(df,type='elg',bathy=T,stations=NULL,reg="",coastline="fine",legend = T) {



  df2 <- df


  # find out if we span the international dateline
  if (diff(range(df$lon, na.rm=T)) > 300) {
    df$lon[df$lon < 0] <- df$lon[df$lon < 0] + 360
    whr_map <- w2hr
  } else {
    whr_map <- whr
  }

  latlim <- range(df$lat,na.rm=T)+diff(range(df$lat,na.rm=T))*c(-.15,.15)
  lonlim <- range(df$lon,na.rm=T)+diff(range(df$lon,na.rm=T))*c(-.15,.15)

  a <- subset(whr_map,long > lonlim[1] & long < lonlim[2] & lat > latlim[1] & lat < latlim[2])

  dat <- getNOAA.bathy(lon1 = lonlim[1], lon2 = lonlim[2], lat1 = latlim[1],lat2 = latlim[2], resolution = 10)


  base_map <- ggplot(a) +
    geom_polygon(aes(x=long, y = lat, group = group)) +
    coord_quickmap(xlim=lonlim, ylim=latlim, expand = F)

  base_map +
    geom_path(data=df,aes(x=lon,y=lat)) +
    theme_bw()

  df <- df2
}


