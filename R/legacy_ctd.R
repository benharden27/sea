#' A function that reads in all .cnv files from a directory
#'
#' @param foldin input folder path
#' @param newFL Prescribe whether new .cnv files have been added or if you can just load the previously sved R file.
#' @param CTDflag List of indices that are to be removed from the list
#' @param plotFL T/F should the CTDs be plotted to folder?
#' @param plotfold The folder where the CTDs are to be plotted
#' @keywords
#' @export
#' @examples
#' readSEActd()
readSEActd <- function(foldin,newFL=T,CTDflag=NULL,plotFL=F,plotfold="~/Desktop/") {
  setwd(foldin)
  if(!file.exists('../CTDs.Rdata') | newFL) {
    files <- list.files(path=foldin, pattern="*.cnv")
    CTDi <- 1:length(files)
    CTDi <- CTDi[!1:length(files)%in%CTDflag]

    CTDs <- NULL
    for (i in 1:length(files)) {

      # Read in CTD and smooth
      CTDs <- append(CTDs,read.ctd(paste(foldin,files[i],sep="/")))
      CTDs[[i]] <- ctdDecimate(ctdTrim(CTDs[[i]],parameters=list(pmin=5)))

      # Get Lat and Lon
      X<-readLatLon(filein=file.path(foldin,files[i]))
      r <- X$r
      lon <- X$lon
      lat <- X$lat

      # Find water depth from cnv
      line <- grep("Water Depth",r)[1]
      depth <- as.numeric(strsplit(r[12],'h')[[1]][2])

      # Assign the longitude and latitude to the appropriate fields in d
      CTDs[[i]]@metadata$longitude <- lon
      CTDs[[i]]@metadata$latitude <- lat
      CTDs[[i]]@metadata$station <- as.numeric(strsplit(files[i],'-')[[1]][2]) # have to do this to make makeSection work
      CTDs[[i]]@metadata$waterDepth <- depth
      CTDs[[i]]@metadata$filename <- files[i]
      CTDs[[i]]@metadata$station <- as.numeric(strsplit(files[i],'-')[[1]][2])

      if(plotFL) {
        filerep <- paste0(strsplit(files[i],'[.]')[[1]][1],".png")
        outname <- file.path(plotfold,"ctds",filerep)
        png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
        plot(CTDs[[i]],which=c(1,2,3,5))
        dev.off()

        filerep <- paste0(strsplit(files[i],'[.]')[[1]][1],"_aux.png")
        outname <- file.path(plotfold,"ctds",filerep)
        png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
        mgp<-getOption("oceMgp")
        par(mfrow=c(2, 2),mar=c(2, mgp[1] + 2, mgp[1] + 1.5, mgp[1]))
        pran <- range(CTDs[[i]]@data$pressure,na.rm=T)

        plot(CTDs[[i]]@data$sigmaTheta,CTDs[[i]]@data$depth,
             ylim=rev(pran),type='l',
             ylab="Depth [m]",xlab="")
        mtext("Density [kg/m3]",line=1,side=3,cex = par("cex"))
        at <- par("xaxp")
        abline(v = seq(at[1], at[2], length.out = at[3] + 1),
               lty=3,col="lightgray")
        at <- par("yaxp")
        abline(h = seq(at[1], at[2], length.out = at[3] + 1),
               lty=3,col="lightgray")

        # PAR
        parslot <- grep('par',names(CTDs[[i]]@data))[1]
        if(!is.na(parslot)) {
          plot(CTDs[[i]]@data[[parslot]],CTDs[[i]]@data$depth,
               ylim=rev(pran),xlim=c(0,max(CTDs[[i]]@data[[parslot]],na.rm=T)),type='l',
               ylab="Depth [m]",xlab="")
          mtext("PAR",line=1,side=3,cex = par("cex"))
          at <- par("xaxp")
          abline(v = seq(at[1], at[2], length.out = at[3] + 1),
                 lty=3,col="lightgray")
          at <- par("yaxp")
          abline(h = seq(at[1], at[2], length.out = at[3] + 1),
                 lty=3,col="lightgray")
        }

        plot(CTDs[[i]]@data$fluorescence,CTDs[[i]]@data$depth,
             ylim=rev(pran),type='l',
             ylab="Depth [m]",xlab="")
        mtext("Fluorescence [V]",line=1,side=3,cex = par("cex"))
        at <- par("xaxp")
        abline(v = seq(at[1], at[2], length.out = at[3] + 1),
               lty=3,col="lightgray")
        at <- par("yaxp")
        abline(h = seq(at[1], at[2], length.out = at[3] + 1),
               lty=3,col="lightgray")

        # Oxygen
        oxyslot <- grep("oxygen",names(CTDs[[i]]@data))[1]
        if(!is.na(oxyslot)) {
          plot(CTDs[[i]]@data[[oxyslot]],CTDs[[i]]@data$depth,
               ylim=rev(pran),type='l',
               ylab="Depth [m]",xlab="")
          mtext("Oxygen",line=1,side=3,cex = par("cex"))
          at <- par("xaxp")
          abline(v = seq(at[1], at[2], length.out = at[3] + 1),
                 lty=3,col="lightgray")
          at <- par("yaxp")
          abline(h = seq(at[1], at[2], length.out = at[3] + 1),
                 lty=3,col="lightgray")

          dev.off()
        }
      }

    }
    save(CTDs,file="../CTDs.Rdata")
  } else {
    load('../CTDs.Rdata')
    CTDi <- 1:length(CTDs)
    CTDi <- CTDi[!1:length(CTDs)%in%CTDflag]
  }


  CTDs <- CTDs[CTDi]
  return(CTDs)

}


#' Read lat and Lon from header of CTD CNV file
#'
#' Function searches reg exp combinations to find the lat and lon of the CTD
#'
#' @param filein .cnv file to be read from
#' @keywords
#' @export
#' @examples
#' readLatLon()
#'
readLatLon <- function(filein) {

  r <- readLines(filein,encoding ='UTF-8')

  # set possible patterns to search for
  patt <- "([0-9]+[^0-9]+[0-9]+[^0-9]+[0-9]*)"
  patt2 <- "([0-9]+[^0-9]+[0-9])"
  patt3 <- "([0-9]+)"

  # LATITUDE
  # switch depending on what format the lon and lat are stored as
  if(length(grep('^.*Lat.*Lon.*$',r)) > 0) {
    case <- 1
    line <- grep('^.*Lat.*Lon.*$',r)
  } else if (length(grep('Lat|Lat',r,ignore.case=T))==0) {
    case <- 2
    line <- 1
  } else {
    case <- 3
    line <- grep("Lat",r,ignore.case=T)[1] # finds the word "Latitude" in r
  }

  # search for the patterns in order
  a <- regexpr(patt,r[line])
  if (a==-1) {
    a <- regexpr(patt2,r[line])
    if(a==-1) {
      a <- regexpr(patt3,r[line])
    }
  }

  # assign the latitude based on the search findings
  lat <- substr(r[line],a,a+attr(a,"match.length")-1)
  lat <- strsplit(lat,"[^0-9\\.]")[[1]]
  lat <- lat[lat!=""] # removes blank sections

  # Define Hemisphere
  # hemi <- substr(r[line],regexpr("[NS]",r[line]),regexpr("[NS]",r[line]))
  hemi <- substr(r[line],regexpr("[NS].{0,5}$",r[line]),regexpr("[NS].{0,5}$",r[line]))
  if(hemi=='S'){
    fac <- -1
  } else {
    fac <- 1
  }

  # depending on the end format of "lat" do various different things to parse the output
  if(length(lat)==1) {
    if(length(strsplit(lat,"\\.")[[1]])<3) {
      lat <- fac*as.numeric(substr(lat,1,2)) + fac * as.numeric(substr(lat,3,100))/60
    } else {
      lat <- strsplit(lat,"\\.")[[1]]
      lat <- fac * as.numeric(lat[1]) + fac * (as.numeric(lat[2])+as.numeric(lat[3])/10)/60
    }
  } else {
    lat <- fac * as.numeric(lat[1]) + fac * as.numeric(lat[2])/60;
  }


  # LONGITUDE

  # again, switch by the format of the line
  if (case==1) {
    rest<- substr(r[line],a+attr(a,"match.length"),100)
  } else if (case==2) {
    rest <- 'xxxxx'
  } else {
    rest <- r[grep("Lon",r,ignore.case = T)[1]]
  }

  # search for the patterns
  a <- regexpr(patt,rest)
  if (a==-1) {
    a <- regexpr(patt2,rest)
    if(a==-1) {
      a <- regexpr(patt3,rest)
    }
  }

  # assign longitude based on patterns
  lon <- substr(rest,a,a+attr(a,"match.length")-1)
  lon <- strsplit(lon,"[^0-9\\.]")[[1]]
  lon <- lon[lon!=""] # removes blank sections

  # Define Hemisphere
  # hemi <- substr(rest,regexpr("[WE]",rest),regexpr("[WE]",rest));
  hemi <- substr(rest,regexpr("[WE].{0,5}$",rest),regexpr("[WE].{0,5}$",rest));
  if(hemi=='W'){
    fac <- -1
  } else {
    fac <- 1
  }


  # do various formating based on type of "lon" output
  if(length(lon)==1) {
    if(length(strsplit(lon,"\\.")[[1]])==2) {
      lon <- fac * as.numeric(substr(lon,1,2)) + fac * as.numeric(substr(lon,3,100))/60
    } else if (nchar(lon)>3) {
      lon <- strsplit(lon,"\\.")[[1]]
      lon <- fac * as.numeric(lon[1]) +fac * (as.numeric(lon[2])+as.numeric(lon[3])/10)/60
    } else {
      lon <- fac*as.numeric(lon)
    }
  } else {
    lon <- fac* as.numeric(lon[1]) + fac * as.numeric(lon[2])/60;
  }

  # show the lines of output for when there is no lon or no lat
  if(is.na(lon)|is.na(lat)) {
    show(r)
    # a<-readline('Press enter key to continue...')
  }

  X <- NULL
  X$lon <- lon
  X$lat <- lat
  X$r <- r

  return(X)
}


#' Read data from an RBR used as a Tow-Yo
#'
#' Function reads in data, cuts it up into casts and outputs.
#' If ELG file is also provided, it can assign these casts
#' to GPS locations.
#'
#' @param filein .dat RBR file to be read from
#' @param elg option .elg file to be read in for locations.
#' @keywords
#' @export
#' @examples
#' readLatLon()
#'
readRBRtow <- function(filein,elg=NULL) {
  r <- readLines(filein,50)
  line <- grep("Cond",r)-1;
  df <- read.table(filein,skip=line,header=T)
  nai <- df$Depth>0.5
  df <- df[nai,]
  df$sigma <- swSigma0(df$Salinity,df$Temp,df$Pres)

  ll <- dim(df)[1]
  sti <- grep("Logging start",r)
  sttime <- tail(strsplit(r[sti]," ")[[1]],2)
  sttime <- paste(sttime[1],sttime[2])
  sttime <- strptime(sttime,"%y/%m/%d %H:%M:%S")
  df$time <- sttime + seq(length.out=ll)*2



  if(!is.null(elg)){
    dfelg <- read.csv(elg,stringsAsFactors = F)
    dfelg2 <- readSEAelg(elg)
    dfelg$datetime <- strptime(paste(dfelg$Date,dfelg$Time),"%m/%d/%Y %H:%M:%S")
    f <- function(val,vec) {which.min(as.numeric(vec-val)^2)}
    sti <- f(df$time[1],dfelg$datetime)
    eni <- f(tail(df$time,1),dfelg$datetime)
    veci <- sti:eni
    df$lon <- approx(veci,dfelg2$lon[veci],seq(sti,eni,length.out=length(df$time)))$y
    df$lat <- approx(veci,dfelg2$lat[veci],seq(sti,eni,length.out=length(df$time)))$y
    df$dist <- geodDist(df$lon,df$lat,alongPath =T)

    dfctd <- as.ctd(df$Salinity,df$Temp,df$Pres,longitude=df$lon,latitude=df$lat)
    dfctds <- ctdFindProfiles(dfctd)

    X <- NULL
    X$df <- df

    ctds <- NULL
    for (i in 1:length(dfctds)) {
      ctds <- append(ctds,ctdDecimate(dfctds[[i]]))
      ctds[[i]]@metadata$longitude <- mean(ctds[[i]]@data$longitude,na.rm=T)
      ctds[[i]]@metadata$latitude <- mean(ctds[[i]]@data$latitude,na.rm=T)
      ctds[[i]]@metadata$station <- i
      # dfctds[[i]]@data$time = mean(dfctds[[i]]@data$time)
      # dfctds[[i]]@data$longitude = mean(dfctds[[i]]@data$longitude)
      # dfctds[[i]]@data$latitude = mean(dfctds[[i]]@data$latitude)
    }
    X$ctds <- ctds
  }

  return(X)

}


#' Calculate 1% light level from CTD PAR data
#'
#' Function reads in a CTD object and returns a
#' modelled 1% light level based on a fitted natural log.
#'
#' @param CTD CTD object created by read.ctd from the oce package
#' @keywords
#' @export
#' @examples
#' opll()
#'
opll <- function(CTD) {

  parslot <- grep('par',names(CTD@data))[1]
  if(length(parslot)==0) {
    stop("No PAR sensor data in CTD object")
  }
  PAR = CTD@data[[parslot]]
  depth = CTD@data$depth

  mincut <- which(PAR<PAR[which(!is.na(PAR))[1]]/200)
  PAR[mincut] <- NA
  depth[depth<20|is.na(depth)] <- NA

  d1 <- tibble(PAR=PAR,depth=depth)

  model1 <- function(a,data) {
    a[1] * log(a[2]/data$PAR)
  }

  measure_distance <- function(mod,data) {
    diff <- data$depth - model1(mod,data)
    sqrt(mean(diff ^ 2,na.rm=T))
  }

  best <- optim(c(10,2000),measure_distance,data=d1)

  d1 <- mutate(d1,mod = model1(best$par,d1))

  ggplot(d1) +
    geom_line(aes(PAR,-mod)) +
    geom_point(aes(PAR,-depth)) +
    coord_cartesian(xlim=c(0,max(d1$PAR,na.rm=T)),ylim=c(-100,0))

  # predicted PAR
  PARpred <- best$par[2]/exp(model1(best$par,d1)/best$par[1])
  oneper <- which(PARpred < best$par[2]/100)[1]
  d1$mod[oneper]




}


