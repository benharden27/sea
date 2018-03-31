########
# PLOTS DATA FROM SEA SOURCES
########

#' Plot Surface Station data
#'
#' Plots data recorded in surfsamp.xls(x/m) once read in to a data frame using readSEAxls
#'
#' Plots are made in subplots determined by the number of variables in vars
#'
#' vars can have following properties:
#'
#' 1 - Nitrates
#'
#' 2 - Phosphates
#'
#' 3 - Extracted Chla
#'
#' 4 - pH
#'
#' 5 - Alkalinity
#'
#' 6 - Oxygen
#'
#' Regs can currently have values prescribed in reg2latlon().
#'
#' @param df Data from created by using readSEAxls().
#' @param vars list of numbers prescribing which fields to plot (see notes).
#' @param reg internally prescribed regions that can be plotted for local maps (see reg2latlon).
#' @keywords
#' @export
#' @examples
#' plotSEAsurf()
plotSEAsurf <- function(df,vars=c(1,2,3,4),reg="") {

  data(coastlineWorld)
  data(coastlineWorldFine)
  par(mgp=getOption("oceMgp"))

  # which to plot?
  lenw <- length(vars)
  if(lenw==1) {rows = c(1,1)}
  if(lenw==2) {rows = c(1,2)}
  if(lenw>2 & lenw<5) {rows = c(2,2)}
  if(lenw>4 & lenw<7) {rows = c(3,2)}
  if(lenw>6) {rows = c(3,3)}

  #
  lon <- df$LonDEC
  lat <- df$LatDEC
  nai <- !is.na(lon) & !is.na(lat)
  lon <- lon[nai]
  lat <- lat[nai]
  lon[lon<0] <- lon[lon<0]+360
  ii <- lon!=0
  lon <- lon[ii]
  lat <- lat[ii]
  df <- df[ii,]

  varnames <- names(df)
  varsearch <- c('no3','po4','chl.*ug','pH','Alk','O2')
  varnames2 <- c('Nitrate [uMol]','Phosphate [uMol]','Chl-a [mg/L]','pH','Alkalinity','Oxygen')
  cm <- oce.colorsDensity
  colmaps <- list(cm,cm,oce.colorsChlorophyll,cm,cm,cm)

  zi <- lapply(vars,function(i) grep(varsearch[i],varnames,ignore.case = T))
  if(length(zi)>1) {
    loci <- rowSums(!is.na(df[,unlist(zi)]))>0
  } else {
    loci <- !is.na(df[,unlist(zi)])>0
  }
  varnames <- varnames2[vars]
  colmaps <- colmaps[vars]

  if(nchar(reg)>0) {
    X <- reg2latlon(reg)
    latlim <- X$latlim
    lonlim <- X$lonlim
  } else {
    latlim <- range(lat[loci],na.rm=T)+diff(range(lat[loci],na.rm=T))*c(-.15,.15)
    lonlim <- range(lon[loci],na.rm=T)+diff(range(lon[loci],na.rm=T))*c(-.15,.15)
  }

  vals <- lon>lonlim[1] & lon<lonlim[2] & lat>latlim[1] & lat<latlim[2]
  df <- df[vals,]
  lon <- lon[vals]
  lat <- lat[vals]

  # set up plotting vars
  latlabels <- seq(-90, 0, 5)
  lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))

  par(mfrow=rows, mar=c(3, 3, 1, 1))
  qua <- c(0.03,0.97)

  for (i in 1:length(zi)) {
    omar <- par('mar')
    if(!length(zi[[i]])) next
    z <- df[[zi[[i]]]]
    if(length(which(!is.na(z)))<2) next
    ran<-quantile(z,qua,na.rm=T)
    # Subset data
    zo<-!is.na(z)
    z<-z[zo]
    lonp<- lon[zo]
    latp<- lat[zo]
    colors <- colormap(z,zlim=ran,col=colmaps[[i]])


    # select colors
    drawPalette(colormap=colors)
    mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
            latitudelim = latlim,
            longitudelim = lonlim,
            col='gray',axes=FALSE, grid=FALSE)
    latlabels <- seq(-90, 0, 5)
    lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
    mapGrid(longitude=lonlabels, latitude=latlabels)
    mapAxis(longitude=lonlabels, latitude=latlabels)
    mapPoints(lonp,latp,pch=21,bg=colors$zcol,cex=1.5)
    # mapLines(lonf,latf)
    mapPoints(lonp,latp,pch=20,cex=0.4)
    # mapText(lon,lat,1:length(lon))
    title(varnames[i])
    par(mar=omar)

  }
}

#' Set or get region data for plotting
#'
#' Function returns lat and lon limits for the plotting tools depending either
#' on the full range of lat and lon, or by prescribing a known region.
#'
#' Local regions currently installed are: 'Suva', 'Nukualofa', 'lau', 'vavau'.
#'
#' If prescribing a vector of length 4, this should be c(lonmin,lonmax,latmin,latmax)
#'
#' @param reg Name of known region or vector of length 4 a blank charactor indicating that function should us the range of lat and lon as bounds.
#' @export
#' @examples
#' reg2latlon()
reg2latlon <- function(reg='') {
  X <- NULL
  if(reg=='suva') {
    X$latlim <- c(-18.3,-18)
    X$lonlim <- c(178.2,178.5)
  } else if (reg=='vavau') {
    X$latlim <- c(-18.88,-18.55)
    X$lonlim <- c(185.6,186.3)
  } else if (reg=='nukualofa') {
    X$latlim <- c(-21.5,-20.5)
    X$lonlim <- c(184.75,185)
  } else if (reg=='lau') {
    X$latlim <- c(-20,-16.5)
    X$lonlim <- c(177,183)
  } else if (is.numeric(reg)&length(reg)==4) {
    X$latlim <- reg[3:4]
    X$lonlim <- reg[1:2]
  }
  return(X)
}


#' Plot plastic distribution from neustron tows data
#'
#' @param df data frame created by using readSEAxls() on <cruiseID_Neuston.xls>.
#' @param legloc location of legend (passed to legend()).
#' @export
#' @examples
#' plotSEAplastic()
plotSEAplastic <- function(df,legloc='topleft',mapPad = 0.1) {
  ii <- grep('Plastic',names(df))
  Plas <- df[[ii[1]]] + df[[ii[2]]]
  lon <- df$LonDEC
  lat <- df$LatDEC
  lon[lon<0] <- lon[lon<0]+360

  mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
          latitudelim = range(lat,na.rm=T)+diff(range(lat,na.rm=T))*c(-mapPad,mapPad),
          longitudelim = range(lon,na.rm=T)+diff(range(lon,na.rm=T))*c(-mapPad,mapPad),
          col='gray',axes=FALSE, grid=FALSE)
  latlabels <- seq(-90, 0, 5)
  lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
  mapGrid(longitude=lonlabels, latitude=latlabels)
  mapAxis(longitude=lonlabels, latitude=latlabels)


  # Plas <- sort.int(Plas,decreasing = T,index.return = T)
  # lon <- lon[Plas[[2]]]
  # lat <- lat[Plas[[2]]]
  # Plas <- Plas[[1]]
  #
  maxval <- max(Plas,na.rm=T)
  minval <- min(Plas,na.rm=T)
  rat <- (Plas-minval)/(maxval-minval)
  rat2 <- rat*16
  minsiz <- 0.2
  maxsiz <- 5
  min2 <- 2^minsiz
  max2 <- 2^maxsiz
  sizedot <- log2(min2 + (max2-min2)*rat)

  # mapPoints(lon[Ni],lat[Ni],bg="gray",pch=21,cex=sizedot[Ni])
  mapPoints(lon,lat,bg="white",pch=21,cex=sizedot)



  legpl <- minsiz:maxsiz
  legnum <- ceiling((maxval-minval)*(2^legpl-min2)/(max2-min2)+minval)
  # lat1<- -24
  # lon1<-180
  # latsp <- .75
  # mapPoints(rep(lon1,length(legpl)),seq(lat1,by=-latsp,length.out=length(legpl)),
  #           bg="white",pch=21,cex=legpl)
  # mapText(lon1,lat1+latsp,expression(pieces),pos=4,offset=1)
  # mapText(rep(lon1,length(legpl)),seq(lat1,by=-latsp,length.out=length(legpl)),
  #         format(legnum,digits=1,scientific=F),pos=4,offset=2)
  # mapPolygon(coastlineWorldFine,col='lightgray')
  # load(file.path(foldmaster,"Rdata/flowthrough.Rdata"))
  # lonf <- data$lon
  # latf <- data$lat
  # mapLines(lonf,latf)
  mapPoints(lon,lat,pch=20,cex=0.4,col=2)
  # mapPoints(lon,lat,pch=20,cex=0.4)
  legend(x=legloc,legend = format(legnum,digits=1,scientific=F),
         pch=21,pt.cex=legpl,x.intersp=2,y.intersp=2,inset=0.05,
         title=' Plastic ',bty='o')


}



#' Plot biomass distribution from neustron tows data
#'
#' @param df data frame created by using readSEAxls() on <cruiseID_Neuston.xls>.
#' @param legloc location of legend (passed to legend()).
#' @export
#' @examples
#' plotSEAbio()
plotSEAbio <- function(df,legloc='topleft',mapPad=0.1) {

  Time <- df$Time.In
  if(is.null(Time)) {
    Time <- df$`Time In`
  }
  tod <- strptime(Time,'%Y-%m-%d %H:%M:%S')[[3]]/24

  lon <- df$LonDEC
  lat <- df$LatDEC
  ii <- grep('Density',names(df))
  biom <- df[ii]


  lon<-lon[!is.na(biom)]
  lat<-lat[!is.na(biom)]
  tod <- tod[!is.na(biom)]
  biom <- biom[!is.na(biom)]


  lon[lon<0] <- lon[lon<0]+360

  Ni <- tod>0.75 | tod<0.25

  latlim <- range(lat,na.rm=T) + c(-diff(range(lat,na.rm=T))*mapPad,diff(range(lat,na.rm=T))*mapPad)
  lonlim <- range(lon,na.rm=T) + c(-diff(range(lon,na.rm=T))*mapPad,diff(range(lon,na.rm=T))*mapPad)
  par(mar=c(3, 3, 1, 1),mgp=getOption("oceMgp"))
  mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
          latitudelim = latlim,
          longitudelim = lonlim,
          col='gray',axes=FALSE, grid=FALSE)
  latlabels <- seq(-90, 0, 5)
  lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
  mapGrid(longitude=lonlabels, latitude=latlabels)
  mapAxis(longitude=lonlabels, latitude=latlabels)

  maxval <- max(biom,na.rm=T)
  minval <- min(biom,na.rm=T)
  rat <- (biom-minval)/(maxval-minval)
  rat2 <- rat*16
  minsiz <- 1
  maxsiz <- 5
  min2 <- 2^minsiz
  max2 <- 2^maxsiz
  sizedot <- log2(min2 + (max2-min2)*rat)

  mapPoints(lon[Ni],lat[Ni],bg="gray",pch=21,cex=sizedot[Ni])
  mapPoints(lon[!Ni],lat[!Ni],bg="white",pch=21,cex=sizedot[!Ni])

  legpl <- minsiz:maxsiz
  legnum <- ceiling(1000*(maxval-minval)*(2^legpl-min2)/(max2-min2)+minval)

  mapPoints(lon,lat,pch=20,cex=0.4)
  legend(x=legloc,legend = format(legnum,digits=1,scientific=F),
         pch=21,pt.cex=legpl,x.intersp=2,y.intersp=2,inset=0.05,
         title=paste0("  ",expression(uL/m^2),"  "),bty='o')


}



#' Plot biodiveristy distribution from neustron tows data
#'
#' @param df data frame created by using readSEAxls() on <cruiseID_Neuston.xls>.
#' @param legloc location of legend (passed to legend()).
#' @export
#' @examples
#' plotSEAbiod()
plotSEAbiod <- function(df,legloc='topleft',mapPad=0.1) {

  Time <- df$Time.In
  if(is.null(Time)) {
    Time <- df$`Time In`
  }
  tod <- strptime(Time,'%Y-%m-%d %H:%M:%S')[[3]]/24

  lon <- df$LonDEC
  lat <- df$LatDEC
  ii <- grep('shannon',names(df))
  biod <- df[ii]


  lon<-lon[!is.na(biod)]
  lat<-lat[!is.na(biod)]
  tod <- tod[!is.na(biod)]
  biod <- biod[!is.na(biod)]


  lon[lon<0] <- lon[lon<0]+360

  Ni <- tod>0.75 | tod<0.25

  latlim <- range(lat,na.rm=T) + c(-diff(range(lat,na.rm=T))*mapPad,diff(range(lat,na.rm=T))*mapPad)
  lonlim <- range(lon,na.rm=T) + c(-diff(range(lon,na.rm=T))*mapPad,diff(range(lon,na.rm=T))*mapPad)
  par(mar=c(3, 3, 1, 1),mgp=getOption("oceMgp"))
  mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
          latitudelim = latlim,
          longitudelim = lonlim,
          col='gray',axes=FALSE, grid=FALSE)
  latlabels <- seq(-90, 0, 5)
  lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
  mapGrid(longitude=lonlabels, latitude=latlabels)
  mapAxis(longitude=lonlabels, latitude=latlabels)


  mapPoints(lon[Ni],lat[Ni],bg="gray",pch=21,cex=biod[Ni]*5)
  mapPoints(lon[!Ni],lat[!Ni],bg="white",pch=21,cex=biod[!Ni]*5)
  mapPoints(lon,lat,pch=20,cex=0.4)
  legend(x=legloc,legend = seq(0.2,1,0.2),
         pch=21,pt.cex=5*seq(0.2,1,0.2),x.intersp=2,y.intersp=2,inset=0.05,
         title='Diversity',bty='o')
}



#' Plot biomass boxplot between day and night from neustron tows data
#'
#' @param df data frame created by using readSEAxls() on <cruiseID_Neuston.xls>.
#' @export
#' @examples
#' plotSEAbiodn()
plotSEAbiodn <- function(df) {

  Time <- df$Time.In
  if(is.null(Time)) {
    Time <- df$`Time In`
  }
  tod <- strptime(Time,'%Y-%m-%d %H:%M:%S')[[3]]/24

  lon <- df$LonDEC
  lat <- df$LatDEC
  ii <- grep('Density',names(df))
  biom <- df[ii]


  lon<-lon[!is.na(biom)]
  lat<-lat[!is.na(biom)]
  tod <- tod[!is.na(biom)]
  biom <- biom[!is.na(biom)]


  lon[lon<0] <- lon[lon<0]+360

  Ni <- tod>0.75 | tod<0.25

  boxplot(list(1000*biom[!Ni],1000*biom[Ni]),
          names=c("Day","Night"),
          ylab="Biomass Density [uL/m^2]",
          boxwex=0.2)

}





#' Plot map of currents from hull-mounted ADCP data
#'
#' @param X object created by readSEAadcp() or readSEAadcp_all().
#' @param stp integer indicating the number of timesteps to skip between subsequent arrows.
#' @param scale double to scale the vector arrows drawn
#' @param plotKEY logical plot or don't plot
#' @param reg option to define a plotting region (calls reg2latlon).
#' @export
#' @examples
#' plotSEAcurr()
plotSEAcurr <- function(X,stp=6,scale=0.2,plotKEY=T,reg="",surf=F,plotCT=T) {

  if(surf) {
    ubar <- rowMeans(X$u[,1:5],na.rm=T)/10
    vbar <- rowMeans(X$v[,1:5],na.rm=T)/10
  } else {
    ubar <- rowMeans(X$u,na.rm=T)/10
    vbar <- rowMeans(X$v,na.rm=T)/10
  }
  porti <- diff(geodDist(X$lon,X$lat,alongPath = T))<0.1
  ubar[porti] <- vbar[porti] <- X$lon[porti] <- X$lat[porti] <- NA
  X$lon <- X$lon[!porti]
  X$lat <- X$lat[!porti]
  X$lon[X$lon==0] <- NA
  X$lat[is.na(X$lon)] <- NA
  ubar <- ubar[!porti]
  vbar <- vbar[!porti]
  ws <- sqrt(ubar^2+vbar^2)
  wd <- 180*atan2(ubar,vbar)/pi
  X$lon[X$lon<0&!is.na(X$lon)] <- X$lon[X$lon<0&!is.na(X$lon)]+360

  wslab <- signif(median(ws,na.rm=T),1)

  if(nchar(reg)>0) {
    Y <- reg2latlon(reg)
    latlim <- Y$latlim
    lonlim <- Y$lonlim
  } else {
    latlim <- range(X$lat,na.rm=T)+diff(range(X$lat,na.rm=T))*c(-.15,.15)
    lonlim <- range(X$lon,na.rm=T)+diff(range(X$lon,na.rm=T))*c(-.15,.15)
  }


  par(mar=c(3, 3, 1, 1),mgp=getOption("oceMgp"))
  mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
          latitudelim = latlim,
          longitudelim = lonlim,
          col='gray',axes=FALSE, grid=FALSE)
  latlabels <- seq(-90, 0, 5)
  lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
  mapGrid(longitude=lonlabels, latitude=latlabels)
  mapAxis(longitude=lonlabels, latitude=latlabels)

  ran <- seq(1,length(X$lon),stp)
  if(plotCT) {
    mapLines(X$lon,X$lat,col=2)
  }
  cols<-colormap(ws)
  mapDirectionField(X$lon[ran],X$lat[ran],ubar[ran],vbar[ran],scale=scale,length=0.01)

  if(plotKEY) {
    lonl <- max(X$lon,na.rm=T)
    latl <- min(X$lat,na.rm=T)
    lonran <- range(X$lon,na.rm=T)
    latran <- range(X$lat,na.rm=T)
    dll <- matrix(NA,2,2)
    for (i in 1:2) {
      for (j in 1:2) {
        dll[i,j] <- min((X$lon - lonran[i])^2 + (X$lat - latran[j])^2,na.rm=T)
      }
    }
    dlli <- which.max(dll)
    dllj <- dlli-(floor(dlli/2))
    dlli <- ceiling(dlli/2)
    a<-lonlat2map(lonran[dlli]+c(1,-1),latran[dllj]+c(.15,-.15))
    # rect(a$x[1],a$y[1],a$x[2],a$y[2],col=0)
    mapDirectionField(lonran[dlli],latran[dllj],wslab,0,scale=scale,length=0.01)
    mapText(lonran[dlli],latran[dllj],paste(wslab,'cm/s'),pos=2)

  }
}




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
plotSEAct <- function(df,type='elg',bathy=T,stations=NULL,reg="",coastline="fine",legend = T) {
  if(type=='elg') {
    lon <- df$lon
    lat <- df$lat
  } else if (type=='hourly') {
    lon <- df$LonDEC
    lat <- df$LatDEC
    lon[lon<0] <- lon[lon<0]+360;
  }

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





#' Plot map of currents from onboard animometer
#'
#' @param df data frame from hourly data. Using readSEAxls() on <cruiseID>_hourlywork.xls
#' @param stp integer indicating the number of timesteps to skip between subsequent arrows.
#' @param scale double to scale the vector arrows drawn
#' @param plotKEY logical plot or don't plot
#' @param reg option to define a plotting region (calls reg2latlon).
#' @export
#' @examples
#' plotSEAwind()
plotSEAwind <- function(df,scale=0.2,stp=3,plotKEY=T,reg="") {
  lon <- df$LonDEC
  lat <- df$LatDEC
  lon[lon<0&!is.na(lon)] <- lon[lon<0&!is.na(lon)]+360;

  # u <- df$Wind.E.W.Comp...m.s.
  u <- df[[grep('Wind.*E.*W',names(df))]]
  # v<- df$Wind.N.S.Comp...m.s.
  u <- df[[grep('Wind.*N.*S',names(df))]]
  # ws<-df$Wind.Speed..knots.
  ws <- df[[grep('Wind.*Speed.*knots',names(df))]]
  # wd<-df$Wind.Direction..deg.
  wd <- df[[grep('Wind.*Direction.*deg',names(df))]]

  u1<- -ws*sin(wd*pi/180)*0.514444
  v1<- -ws*cos(wd*pi/180)*0.514444
  ws[is.na(df$Temp)]<-wd[is.na(df$Temp)]<-u1[is.na(df$Temp)]<-v1[is.na(df$Temp)]<-NA
  wslab <- signif(median(ws,na.rm=T),1)

  if(nchar(reg)>0) {
    Y <- reg2latlon(reg)
    latlim <- Y$latlim
    lonlim <- Y$lonlim
  } else {
    latlim <- range(lat,na.rm=T)+diff(range(lat,na.rm=T))*c(-.15,.15)
    lonlim <- range(lon,na.rm=T)+diff(range(lon,na.rm=T))*c(-.15,.15)
  }

  mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
          latitudelim = latlim,
          longitudelim = lonlim,
          col='gray',axes=FALSE, grid=FALSE)
  latlabels <- seq(-90, 0, 5)
  lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
  mapGrid(longitude=lonlabels, latitude=latlabels)
  mapAxis(longitude=lonlabels, latitude=latlabels)

  ran <- seq(1,length(lon),stp)
  mapLines(lon,lat,col=2)
  mapDirectionField(lon[ran],lat[ran],u1[ran],v1[ran],scale=scale,length=0.01)

  if(plotKEY) {
    lonran <- range(lon,na.rm=T)
    latran <- range(lat,na.rm=T)
    dll <- matrix(NA,2,2)
    for (i in 1:2) {
      for (j in 1:2) {
        dll[i,j] <- min((lon - lonran[i])^2 + (lat - latran[j])^2,na.rm=T)
      }
    }
    dlli <- which.max(dll)
    dllj <- ceiling(dlli/2)
    dlli <- 2-(dlli %% 2)
    a<-lonlat2map(lonran[dlli]+c(1,-1),latran[dllj]+c(.15,-.15))
    # rect(a$x[1],a$y[1],a$x[2],a$y[2],col=0)
    mapDirectionField(lonran[dlli],latran[dllj],wslab,0,scale=scale,length=0.01)
    mapText(lonran[dlli],latran[dllj],paste(wslab,'m/s'),pos=2)

  }


}


#' Plot flowthrough data stored in an ELG file
#'
#'Takes data loaded in from an ELG file and plots a map of a number of flowthrough variables along the cruise track
#'
#'Notes: vars can be:
#'
#'1 - Temperature
#'
#'2 - Salinity
#'
#'3 - Fluoroescence
#'
#'4 - CDOM Fluoroescence
#'
#' @param df data frame produced by using readSEAelg().
#' @param vars options for which variables to plot
#' @param step gap between subsequent plotted points
#' @param reg option to define a plotting region (calls reg2latlon).
#' @param bathy logical for including background bathymetry (FIX: CURRENTLY STORED LOCALLY)
#' @export
#' @examples
#' plotSEAelg()
plotSEAelg <- function(df,vars=c(1,2,3,4),step=60,reg="",bathy=T,new_elg=F) {
  data(coastlineWorld)
  data(coastlineWorldFine)
  par(mgp=getOption("oceMgp"))

  # Read in Bathymetry
  if(bathy) {
    load("~/Documents/projects/SEA/data/etopo5")
    lonran = range(df$lon,na.rm=T)
    latran = range(df$lat,na.rm=T)
    lonlim <- lonran + c(-10,10) #-360?
    latlim <- latran + c(-10,10)
    topo <- oce::subset(topo, latlim[1] < latitude & latitude < latlim[2])
    if(lonran[2]>180) {
      topo@data$longitude <- topo@data$longitude + 360
    }
    topo <- oce::subset(topo, lonlim[1] < longitude & longitude < lonlim[2])
    if(lonran[2]<180) {
      topo@data$longitude <- topo@data$longitude + 360
    }
  }

  # set up plotting vars
  breaks <- c(-10000,-7000,-5000,-4000,-3000,-2000,-1000,-500,-200,-100,0)
  latlabels <- seq(-90, 0, 5)
  lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))

  # which to plot?
  lenw <- length(vars)
  if(lenw==1) {rows = c(1,1)}
  if(lenw==2) {rows = c(2,1)}
  if(lenw>2 & lenw<5) {rows = c(2,2)}
  if(lenw>4 & lenw<7) {rows = c(2,3)}
  if(lenw>6) {rows = c(3,3)}

  if(nchar(reg)>0) {
    Y <- reg2latlon(reg)
    latlim <- Y$latlim
    lonlim <- Y$lonlim
  } else {
    latlim <- range(df$lat,na.rm=T)+diff(range(df$lat,na.rm=T))*c(-.15,.15)
    lonlim <- range(df$lon,na.rm=T)+diff(range(df$lon,na.rm=T))*c(-.15,.15)
  }
  ti <- df$lon>lonlim[1]-1 & df$lon<lonlim[2]+1 & df$lat>latlim[1]-1 & df$lat<latlim[2]+1

  par(mfrow=rows, mar=c(3, 3, 1, 1))
  omar <- par('mar')
  for (i in vars) {
    if (i==1) {
      if(new_elg){
        z<-df$temp[ti]
      } else {
        z<-df$Tsal.temp[ti]
      }

      qua <- c(0.04,0.96)
      zl <- 'temperature [degC]'
      cm <- oce.colorsTemperature
    } else if (i==2) {
      if(new_elg) {
        z <- df$sal[ti]
      } else {
        z<-df$Tsal.sal[ti]
      }
      qua <- c(0.02,0.99)
      zl <- 'salinity'
      cm <- oce.colorsSalinity
    } else if (i==3) {
      if(new_elg){
        z <- df$fluor_60min[ti]
      } else {
        z<-df$Fluor.Chl.avg.60.min.Value[ti]
      }
      qua <- c(0.01,0.97)
      zl <- 'chlorophyll-a fluorescence'
      cm <- oce.colorsChlorophyll
    } else if (i==4) {
      if(new_elg) {
        z <- df$CDOM_60min[ti]
      } else {
        ii <- grep('CDOM.*60',names(df))
        z<-df[[ii]][ti]
      }

      qua <- c(0.01,0.92)
      zl <- 'CDOM fluorescence'
      cm<-oce.colorsChlorophyll
    } else if (i==5) {
      if(new_elg) {
        z <- df$xmiss_60min[ti]
      } else {
        ii <- grep('Trans.*60',names(df))
        z<-df[[ii]][ti]
      }

      qua <- c(0.1,0.92)
      zl <- 'Transmissivity'
      cm<-oce.colorsDensity()
    }

    lonp <- df$lon[ti]
    latp <- df$lat[ti]
    ran <- quantile(z,qua,na.rm=T)
    zi<-!is.na(z)
    z<-z[zi]
    lonp<- lonp[zi]
    latp<- latp[zi]

    par(mar=omar)
    colors <- colormap(z,zlim=ran,col=cm)
    drawPalette(colormap=colors)

    mapPlot(coastlineWorld, type='l',proj="+proj=merc +lon_0=180",
            longitudelim=lonlim, latitudelim=latlim,
            axes=FALSE, grid=FALSE)
    if(bathy){
      mapImage(topo,filledContour=TRUE, col=oce.colorsGebco, breaks=breaks)
    }
    mapPoints(lonp[seq(1,length(lonp),step)],latp[seq(1,length(lonp),step)],col=colors$zcol[seq(1,length(lonp),step)],pch=16)
    title(zl)
    mapPolygon(coastlineWorldFine,col='gray')
    mapGrid(longitude=lonlabels, latitude=latlabels)
    mapAxis(longitude=lonlabels, latitude=latlabels)

    #mapPoints(lon[seq(1,length(lon),step)],lat[seq(1,length(lon),step)],col=colors$zcol[seq(1,length(lon),step)],pch=16)
    par(mar=omar)
  }
}





#' Plot CTD sections as map and two panels of T and S
#'
#' @param CTDs object containing CTD objects created using read.ctd from oce package.
#' @param ylim range of values for y axis.
#' @export
#' @examples
#' plotmapTS()
plotmapTS<-function(CTDs,ylim=c(0,1000)) {
  # Make the whole Section and read metadata
  sec <- as.section(CTDs)
  # dist <- geodDist(sec,alongPath=T)
  s <- sectionGrid(sec)
  nstation <- length(s[['station']])
  p <- unique(s[['pressure']])
  d <- swDepth(p,mean(s@metadata$latitude,na.rm=T))

  # Set up T and S arrays for plotting
  np <- length(p)
  Te <- S <- array(NA, dim=c(nstation, np))
  lonctd <- latctd <- rep(NA,nstation)
  for (i in 1:nstation) {
    Te[i, ] <- s[['station']][[i]][['temperature']]
    S[i, ] <- s[['station']][[i]][['salinity']]
    lonctd[i] <- s[["station"]][[i]]@metadata$longitude
    latctd[i] <- s[["station"]][[i]]@metadata$latitude
  }

  dist <- geodDist(lonctd,latctd,alongPath=T)
  lonctd[lonctd<0] <- lonctd[lonctd<0]+360;


  # Make appropriate axes
  nch <- 8 # min desired number of levels

  # Temerature
  rnd <- 0.5
  Tinc <- c(2,1,0.5,0.2,0.1,0.05) # decending order
  Tran <- c(floor(min(Te,na.rm=T)/rnd)*rnd,ceiling(max(Te,na.rm=T)/rnd)*rnd)
  Tinc <- Tinc[which(diff(Tran)/Tinc>nch)[1]]
  Tran <- seq(Tran[1], Tran[2], Tinc)

  # Salinity
  rnd <- 0.1
  Sinc <- c(0.5,0.2,0.1,0.05,0.02,0.01) # decending order
  Sran <- c(floor(quantile(S,0.01,na.rm=T)/rnd)*rnd,ceiling(quantile(S,0.99,na.rm=T)/rnd)*rnd)
  Sinc <- Sinc[which(diff(Sran)/Sinc>nch)[1]]
  Sran <- seq(Sran[1], Sran[2], Sinc)

  # Create the colormaps
  Tcm <- colormap(Te, breaks=Tran, col=oceColorsTemperature)
  Scm <- colormap(S, breaks=Sran, col=oceColorsSalinity)

  # Sets up the layout
  layout(matrix(c(1,2,3),3,1),heights=c(1.5,1,1))

  # Plot the map
  par(pty='s') # square plot

  # find range of lon/lat
  lonran <- diff(range(lonctd,na.rm=T))/10
  latran <- diff(range(latctd,na.rm=T))/10

  if(sum(lonctd>180)==0) {
    mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
            latitudelim = range(latctd,na.rm=T)+c(-latran,latran),
            longitudelim = range(lonctd,na.rm=T)+c(-lonran,lonran),
            col='gray')
  } else {
    mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
            latitudelim = range(latctd,na.rm=T)+c(-latran,latran),
            longitudelim = range(lonctd,na.rm=T)+c(-lonran,lonran),
            col='gray',grid=FALSE)
    latlabels <- seq(-90, 0, 5)
    lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
    mapGrid(longitude=lonlabels, latitude=latlabels)
    mapAxis(longitude=lonlabels, latitude=latlabels)
  }


  # Plot stations, path and labels
  mapLines(lonctd,latctd)
  mapPoints(lonctd,latctd,col=2)
  distmin <- tail(dist,1)/(length(dist)+1)
  cur <- 1
  for (i in 1:length(dist)) {
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mapText(lonctd[i],latctd[i],s@metadata$stationId[i],pos=4)
      cur <- i
    }
  }


  # Plots the sections
  # Change aspect ratio of plots
  par(pty='m')

  # Plot temperature and add labels and profile lines
  imagep(dist, p, Te, colormap=Tcm, flipy=TRUE,ylab='depth [m]',
         zlab='temperature [degC]',missingColor = NULL,
         drawTriangles = T, ylim = c(0,1000),
         zlabPosition = 'side',filledContour=TRUE)
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray')
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }

  # Plot temperature and add labels and profile lines
  imagep(dist, p, S, colormap=Scm, flipy=TRUE,xlab='distance [km]', ylab='depth [m]',
         filledContour=TRUE,zlab='salinity',missingColor = NULL,
         drawTriangles = T, ylim = ylim,
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray')
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }

}



#' Plot CTD sections as map and three panels of T, S and rho
#'
#' @param CTDs object containing CTD objects created using read.ctd from oce package.
#' @param ylim range of values for y axis.
#' @param Tran Range of Temperature
#' @param Sran Range of Salinity
#' @param Dran Range of Potential Density
#' @export
#' @examples
#' plotmap3sec()
plotmap3sec<-function(CTDs,ylim=c(0,300),Tran=NULL,Sran=NULL,Dran=NULL, dist_vec = NULL) {
  # Make the whole Section and read metadata
  sec <- as.section(CTDs)
  s <- sectionGrid(sec)
  nstation <- length(s[['station']])
  p <- unique(s[['pressure']])
  d <- swDepth(p,mean(s@metadata$latitude,na.rm=T))

  # Set up T and S arrays for plotting
  np <- length(p)
  Te <- S <- Den <- array(NA, dim=c(nstation, np))
  lonctd <- latctd <- rep(NA,nstation)
  for (i in 1:nstation) {
    Te[i, ] <- s[['station']][[i]][['temperature']]
    S[i, ] <- s[['station']][[i]][['salinity']]
    Den[i, ] <- s[['station']][[i]][['sigmaTheta']]
    lonctd[i] <- s[["station"]][[i]]@metadata$longitude
    latctd[i] <- s[["station"]][[i]]@metadata$latitude
  }

  if(is.null(dist_vec)) {
    dist <- geodDist(lonctd,latctd,alongPath=T)
  } else {
    dist <- dist_vec
  }

  lonctd[lonctd<0] <- lonctd[lonctd<0]+360;


  # Make appropriate axes
  nch <- 8 # min desired number of levels

  # Temerature
  rnd <- 0.5
  Tinc <- c(2,1,0.5,0.2,0.1,0.05) # decending order
  if(is.null(Tran)) {
    Tran <- c(floor(min(Te,na.rm=T)/rnd)*rnd,ceiling(max(Te,na.rm=T)/rnd)*rnd)
  }
  Tinc <- Tinc[which(diff(Tran)/Tinc>nch)[1]]
  Tran <- seq(Tran[1], Tran[2], Tinc)

  # Salinity
  rnd <- 0.1
  Sinc <- c(2,1,0.5,0.2,0.1,0.05,0.02,0.01) # decending order
  if(is.null(Sran)) {
    Sran <- c(floor(quantile(S,0.01,na.rm=T)/rnd)*rnd,ceiling(quantile(S,0.99,na.rm=T)/rnd)*rnd)
  }
  Sinc <- Sinc[which(diff(Sran)/Sinc>nch)[1]]
  Sran <- seq(Sran[1], Sran[2], Sinc)

  # Density
  rnd <- 0.1
  Dinc <- c(0.5,0.2,0.1,0.05,0.02,0.01) # decending order
  if (is.null(Dran)) {
    Dran <- c(floor(quantile(Den,0.01,na.rm=T)/rnd)*rnd,ceiling(quantile(Den,0.99,na.rm=T)/rnd)*rnd)
  }
  Dinc <- Dinc[which(diff(Dran)/Dinc>nch)[1]]
  Dran <- seq(Dran[1], Dran[2], Dinc)

  # Create the colormaps
  Tcm <- colormap(Te, breaks=Tran, col=oceColorsTemperature)
  Scm <- colormap(S, breaks=Sran, col=oceColorsSalinity)
  Dcm <- colormap(Den, breaks=Dran, col=oceColorsDensity)

  # Sets up the layout
  layout(matrix(c(1,1,1,2,3,4,2,3,4),3,3))

  # Plot the map
  par(pty='s') # square plot
  mapPlot(coastlineWorldFine,proj="+proj=merc +lon_0=180",
          latitudelim = range(latctd,na.rm=T)+c(-.2,.2),
          longitudelim = range(lonctd,na.rm=T)+c(-.2,.2),
          col='gray')
  # latlabels <- seq(-90, 0, 5)
  # lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
  # mapGrid(longitude=lonlabels, latitude=latlabels)
  # mapAxis(longitude=lonlabels, latitude=latlabels)

  # Plot stations, path and labels
  mapLines(lonctd,latctd)
  mapPoints(lonctd,latctd,col=2)
  distmin <- tail(dist,1)/(length(dist)+1)
  cur <- 1
  for (i in 1:length(dist)) {
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mapText(lonctd[i],latctd[i],s@metadata$stationId[i],pos=4)
      cur <- i
    }
  }


  # Plots the sections
  # Change aspect ratio of plots
  par(pty='m')

  # Plot temperature and add labels and profile lines
  imagep(dist, p, Te, colormap=Tcm, flipy=TRUE,ylab='depth [m]',
         filledContour=TRUE,zlab='temperature [degC]',missingColor = NULL,
         drawTriangles = T, ylim = ylim,
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray')
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }

  # Plot temperature and add labels and profile lines
  imagep(dist, p, S, colormap=Scm, flipy=TRUE,xlab='distance [km]', ylab='depth [m]',
         filledContour=TRUE,zlab='salinity',missingColor = NULL,
         drawTriangles = T, ylim = ylim,
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray')
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }

  # Plot temperature and add labels and profile lines
  imagep(dist, p, Den, colormap=Dcm, flipy=TRUE,xlab='distance [km]', ylab='depth [m]',
         filledContour=TRUE,zlab='density',missingColor = NULL,
         drawTriangles = T, ylim = ylim,
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray')
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }


}

#' Plot CTD auxilary data sections (fluo/oxygen) as two panels
#'
#' @param CTDs object containing CTD objects created using read.ctd from oce package.
#' @export
#' @examples
#' plot02flsec()
plotO2flsec<-function(CTDs, dist_vec = NULL) {
  # Make the whole Section and read metadata
  sec <- as.section(CTDs)
  if(is.null(dist_vec)) {
    dist <- geodDist(sec,alongPath=T)
  } else {
    dist <- dist_vec
  }


  s <- sectionGrid(sec)
  nstation <- length(s[['station']])
  p <- unique(s[['pressure']])
  d <- swDepth(p,mean(s@metadata$latitude,na.rm=T))

  # Set up T and S arrays for plotting
  np <- length(p)
  Te <- S <- array(NA, dim=c(nstation, np))
  lonctd <- latctd <- rep(NA,nstation)
  Te <- S <- array(NA, dim=c(nstation, np))
  for (i in 1:nstation) {
    if(!is.null(s[['station']][[i]][['fluorescence']])) {
      Te[i, ] <- s[['station']][[i]][['fluorescence']]
    }
    if(!is.null(s[['station']][[i]][['oxygenConcentrationMole']])) {
      S[i, ] <- s[['station']][[i]][['oxygenConcentrationMole']]
    } else if (!is.null(s[['station']][[i]][['oxygen']])) {
      S[i, ] <- s[['station']][[i]][['oxygen']]
    }
    lonctd[i] <- s[["station"]][[i]]@metadata$longitude
    latctd[i] <- s[["station"]][[i]]@metadata$latitude
  }
  lonctd[lonctd<0] <- lonctd[lonctd<0]+360;

  nch <- 8 # min desired number of levels

  rnd <- 0.001
  Tinc <- c(0.5,0.2,0.1,0.05,0.02,0.01,0.005,0.002,0.001) # decending order
  Tran <- c(floor(min(Te,na.rm=T)/rnd)*rnd,ceiling(quantile(Te,0.99,na.rm=T)/rnd)*rnd)
  Tinc <- Tinc[which(diff(Tran)/Tinc>nch)[1]]
  Tran <- seq(Tran[1], Tran[2], Tinc)

  rnd <- 1
  Sinc <- c(5,2,1,0.5,0.2,0.1) # decending order
  Sran <- c(floor(quantile(S,0.01,na.rm=T)/rnd)*rnd,ceiling(quantile(S,0.99,na.rm=T)/rnd)*rnd)
  Sinc <- Sinc[which(diff(Sran)/Sinc>nch)[1]]
  Sran <- seq(Sran[1], Sran[2], Sinc)

  par(mfrow=c(2, 1))
  Tcm <- colormap(Te, breaks=Tran, col=oceColorsChlorophyll)
  Scm <- colormap(S, breaks=Sran, col=oceColorsSalinity)
  distmin <- tail(dist,1)/(length(dist)+1)

  imagep(dist, p, Te, colormap=Tcm, flipy=TRUE,ylab='depth [m]',
         filledContour=TRUE,zlab='chl-a fluorescence [Volts]',missingColor = NULL,
         drawTriangles = T, ylim = c(0,300),
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray')
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }
  imagep(dist, p, S, colormap=Scm, flipy=TRUE,xlab='distance [km]', ylab='depth [m]',
         filledContour=TRUE,zlab='oxygen conc. [Mol/L]',missingColor = NULL,
         drawTriangles = T, ylim = c(0,300),
         zlabPosition = 'side')
  cur <- 1
  for (i in 1:length(dist)){
    lines(dist[c(i,i)],c(0,max(CTDs[[i]][['depth']],na.rm=T)),col='gray')
    if (i==1 | geodDist(lonctd[cur],latctd[cur],lonctd[i],latctd[i])>distmin) {
      mtext(s@metadata$stationId[i],3,0,at=dist[i])
      cur <- i
    }
  }
}



#' Plot section from RBR Tow-Yo data
#'
#' @param df data frame create using readRBRtow()
#' @export
#' @examples
#' plotRBRtow()
plotRBRtow <- function(df) {


  dran <- range(df$Depth,na.rm=T)

  par(mfrow=c(3,1), mar=c(3, 3, 1, 1))
  omar <- par("mar")

  distloc <- grep('dist',names(df))
  if(length(distloc)==0) {
    xval <- df$time
    xlab <- "Time"
  } else {
    xval <- df$dist
    xlab <- "Distance [km]"
  }

  cm <- oce.colorsSalinity
  ran <- range(df$Salinity,na.rm=T)
  colors <- colormap(df$Salinity,zlim=ran,col=cm)
  drawPalette(colormap=colors)
  plot(xval,df$Depth,col=colors$zcol,pch=19,xlim=range(xval,na.rm=T),ylim=c(dran[2]+5,0),
       xlab=xlab,ylab="Depth [m]")

  par(mar=omar)
  cm <- oce.colorsTemperature
  ran <- range(df$Temp,na.rm=T)
  colors <- colormap(df$Temp,zlim=ran,col=cm)
  drawPalette(colormap=colors)
  plot(xval,df$Depth,col=colors$zcol,pch=19,xlim=range(xval,na.rm=T),ylim=c(dran[2]+5,0),
       xlab=xlab,ylab="Depth [m]")

  par(mar=omar)
  cm <- oce.colorsDensity
  ran <- range(df$sigma,na.rm=T)
  colors <- colormap(df$sigma,zlim=ran,col=cm)
  drawPalette(colormap=colors)
  plot(xval,df$Depth,col=colors$zcol,pch=19,xlim=range(xval,na.rm=T),ylim=c(dran[2]+5,0),
       xlab=xlab,ylab="Depth [m]")
}


#' Plot Bottle data from Hydrocasts
#'
#' Currently plots Nitrate, Phosphate and Chl-a data
#'
#' Notes: which parameters:
#'
#' 1 - Chl-a
#'
#' 2 - Nitrate
#'
#' 3 - Phosphate
#'
#' @param df data frome created using readSEAxls() on <cruiseID>_hyrdoworl.xls
#' @param CTDs location of CTD files for comparisson (not needed)
#' @param plotmap logical to indicate whether to plot a map
#' @param which list of parameters to plot
#' @export
#' @examples
#' plothydro()
plothydro <- function(df,CTDs=NULL,plotmap=T,which=NULL) {

  lon <- df$LonDEC
  lat <- df$LatDEC
  lonstat <- !duplicated(lon) & !duplicated(lat)
  sti <- which(lonstat==T)
  eni <- c(sti[2:length(sti)]-1,length(sti))
  if(!is.null(which)) {
    keep <- NULL
    for (i in which) {
      keep <- append(keep,sti[i]:eni[i])
    }
    df <- df[keep,]
    lon <- df$LonDEC
    lat <- df$LatDEC
    lonstat <- !duplicated(lon) & !duplicated(lat)
  }

  dist = geodDist(lon[lonstat],lat[lonstat],alongPath=T)
  dist = geodDist(lon,lat,alongPath=T)
  if (is.null(CTDs)) {
    stats <- df$Station[lonstat]
    cruiseID <- strsplit(stats[1],'-')[[1]][1]
    foldin <- file.path("~/data/SEA/",cruiseID,'CTD','Cnv')
    CTDs <- readSEActd(foldin,plotFL=F,newFL=F)
  }

  lon[lon<0] <- lon[lon<0]+360

  ii <- grep('Z*.Corr',names(df))
  depth <- df[[ii]]
  if(plotmap) {
    layout(matrix(c(1,1,1,2,3,4,2,3,4),3,3))
    par(mar=c(3,3,1,1),pty="s")
    data("coastlineWorldFine")
    data("coastlineWorld")
    lonm <- lon[lonstat]
    latm <- lat[lonstat]
    stats <- df$Station[lonstat]
    f <- function(x) {strsplit(x,'-|_')[[1]][2]}
    stats <- unlist(lapply(stats,f))

    lonlim <- range(lonm,na.rm=T) + diff(range(lonm,na.rm=T))*c(-.15,.15)
    latlim <- range(latm,na.rm=T) + diff(range(latm,na.rm=T))*c(-.15,.15)

    latlabels <- seq(-90, 0, 5)
    lonlabels <- c(seq(0, 175, 5), seq(-180, 0, 5))
    mapPlot(coastlineWorldFine, type='l',proj="+proj=merc +lon_0=180",
            longitudelim=lonlim, latitudelim=latlim,
            axes=FALSE, grid=FALSE)
    mapPoints(lonm,latm,col=2)
    mapText(lonm,latm,stats,pos=4)
    mapPolygon(coastlineWorldFine,col='gray')
    mapGrid(longitude=lonlabels, latitude=latlabels)
    mapAxis(longitude=lonlabels, latitude=latlabels)
  } else {
    par(mfrow=c(3, 1), mar=c(3, 3, 1, 1),pty="m")
  }
  omar <- par("mar")

  vars = c(1,2,3)
  for (i in vars) {
    if (i==1) {
      ii <- grep('Chl.*ug',names(df))
      z <- df[[ii]]
      cm <- oce.colorsChlorophyll()
      ran <- range(z,na.rm=T)
      main <- 'Chlorophyll-a concentration (mg/L)'
    } else if (i==2) {
      ii <- grep('Nit.*uM',names(df))
      z <- df[[ii]]
      cm <- oceColorsDensity()
      ran <- quantile(z,c(0.01,0.83),na.rm=T)
      main <- 'Nitrate Conc. (uMOL)'
    } else if (i==3) {
      ii <- grep('PO4.*uM',names(df))
      z <- df[[ii]]
      cm <- oceColorsDensity()
      ran <- quantile(z,c(0.01,0.83),na.rm=T)
      main <- 'Phosphate Conc. (uMOL)'
    }

    par(mar=omar,pty="m")
    nai <- !is.na(z)
    colors <- colormap(z,zlim=ran,col=cm)
    drawPalette(colormap=colors)
    plot(dist[nai],depth[nai],pch=21,bg=colors$zcol[nai],cex=1.5,
         ylim=c(600,-10),xlim=range(dist),ylab='Depth (m)',xlab='Distance (km)',main=main)
    text(dist[lonstat],550,stats,adj=0,srt=90)

    }

}


