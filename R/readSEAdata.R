#######
# Read SEA data types
#######

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


#' Extract just the lat and lon of the CTD profiles in a CTD list object
#'
#' Mostly for use in plotting locations of CTDs on cruise map
#'
#' @param CTDs CTD list object
#' @param X previousy extracted locations from another data source (e.g. Neustron tows)
#' @keywords
#' @export
#' @examples
#' readCTDsll()
readCTDsll <- function(CTDs,X=NULL) {
  for (i in 1:length(CTDs)) {
    X$lon <- append(X$lon,CTDs[[i]]@metadata$longitude)
    X$lat <- append(X$lat,CTDs[[i]]@metadata$latitude)
    X$flag <- append(X$flag,1)
  }
  return(X)
}



#' Extract just the lat and lon of the neuston tows stored in a data frame
#'
#' Mostly for use in plotting locations of neustons on cruise map
#'
#' @param df data frame containing Neuston tow data
#' @param X previousy extracted locations from another data source (e.g. CTDs)
#' @keywords
#' @export
#' @examples
#' readbioll()
readbioll <- function(df,X=NULL) {
  X$lon <- c(X$lon,df$LonDEC)
  X$lat <- c(X$lat,df$LatDEC)
  X$flag <- c(X$flag,rep(2,length(X$lon)))
  return(X)
}




#' Read in adcp data files in the format exported before conversion to ODV
#'
#' This needs to be updated to instread read directly from the .ENS files to skip steps in on-board processing
#'
#' @param filein .txt file to read from
#' @keywords
#' @export
#' @examples
#' readSEAadcp()
#'
readSEAadcp <- function(filein) {

  # reads in the file as individual lines
  r <- readLines(filein)

  # finds the header line index and return headerline
  hi <- grep('^Ens.*Eas',r)
  headline <- r[hi]

  # find the indexes of u, v, lon and lat from titles in headerline
  ui <- grep('Eas',strsplit(headline,'\t')[[1]])
  vi <- grep('Nor',strsplit(headline,'\t')[[1]])
  loni <- grep('FLon',strsplit(headline,'\t')[[1]])
  lati <- grep('FLat',strsplit(headline,'\t')[[1]])

  # read the table in from 3 lines past the header onwards
  a<-read.table(filein,skip=hi+3,header=FALSE,sep='\t')

  # extract u and v, lon and lat
  u <- as.matrix(a[ui])
  v <- as.matrix(a[vi])
  lon <- c(a[loni])[[1]]
  lat <- c(a[lati])[[1]]

  # ensure that u and v are output as numeric
  u <- apply(u,2,as.numeric)
  v <- apply(v,2,as.numeric)
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)

  return(list(u=u,v=v,lon=lon,lat=lat))
}


#'
#' As readSEAadcp, this needs to be updated to instread read directly from the .ENS files to skip steps in on-board processing
#'
#' @param foldin folder containing all the .txt file to read from
#' @keywords
#' @export
#' @examples
#' readSEAadcp_all()
#'
readSEAadcp_all <- function(foldin) {
  files <- list.files(path=foldin, pattern="^ADCP.*30.txt$")
  comp <- unlist(strsplit(gsub(cruiseID,paste(cruiseID,'-',sep=''),files),'[-_]'))
  ii <- grep(cruiseID,comp)
  filenum <- as.numeric(comp[ii+1])
  startnum <- as.numeric(comp[ii+2])
  ord <- sort(filenum+startnum/(max(startnum)+1),index.return=T)
  files <- files[ord$ix]

  u <- v <- lon <- lat <- NULL

  for (i in 1:length(files)) {
    cat(i)
    filein <- file.path(foldin,files[i])
    cat(filein,"\n")
    X <- readSEAadcp(filein)
    u<-rbind(u,X$u)
    v<-rbind(v,X$v)
    lon<-append(lon,X$lon)
    lat<-append(lat,X$lat)
  }

  lon[lon<0&!is.na(lon)] <- lon[lon<0&!is.na(lon)] +360

  return(list(u=u,v=v,lon=lon,lat=lat))
}





#' Read in SEA data from an Excel spreadsheet
#'
#' Function used commonly as much data entry and organization is acheived in this format
#'
#' Notes: Requires readxl package. readxl is slower than read.table so the function creates
#' a .csv file of the output for quicker reading next time. Can be overridded using rplcsv.
#'
#' @param filein .xls, .xlsm, .xlsx file to read from
#' @param skip Number of rows to skip at the top of the xls file before reading
#' @param sheet Number of the sheet in the file to be read
#' @param rplcsv Function will create a csv file in the same directory as the
#' xls file for quicker reading next time. rplcsv will override whether this function
#' will overwrite that or look for it and read from it.
#' @keywords
#' @export
#' @examples
#' # Read in an example neuston datasheet
#' df <- readSEAxls(system.file('extdata','S275_Neuston.xlsm',package='sea'))
#'
readSEAxls <- function(filein,skip=0,sheet=1,rplcsv=FALSE) {
  # Determine the extension of the file
  # NB: these can be different between Cramer and Seamans
  ext <- tools::file_ext(filein)

  # create csv output filename
  filecsv <- gsub(ext,"csv",filein)

  # Checks to see if a csv file already exists or if rplcsv == T
  # If it doesn't or the function is forced to rewrite then readxl package
  # used to read the excel file and output to csv file in same folder. Otherwise
  # we just read in the csv file [quicker]
  if(!file.exists(filecsv) | rplcsv==TRUE) {

    # Read in the excel file
    df<-readxl::read_excel(filein,sheet=sheet,skip=skip)

    # Convert from Tibble [FIX: work out if tibble is prefereable]
    df <- as.data.frame(df)

    # Add dots to names
    names(df) <- sub(" ",".",names(df))


    # SEA excel files often have unused extra row so we have to remove them
    nrows <- which(is.na(df[1]))[1]-1
    if(!is.na(nrows)) {
      df <- df[1:nrows,]
    }

    # add a filename to df attributes
    attr(df,"filename") <- filein

    # write the csv file to disk
    write.csv(df,filecsv,row.names = F)

  } else {

    # Read in csv file from folder where excel file exists
    df <- readSEAcsv(filecsv)
    attr(df,"filename") <- filein

  }

  return(df)
}




#' Read in SEA data from a CSV file
#'
#' Function used commonly as quick way of taking SEA data previously
#' converted from xls to csv using readSEAxls()
#'
#' Notes: Is called automatically from readSEAxls() if the csv file is located in the same
#' directory as the xls file, unless rplcsv is set to T.
#'
#' @param filein .csv file to read from
#' @keywords
#' @export
#' @examples
#' readSEAcsv()
#'
readSEAcsv <- function(filein) {
  df <- read.csv(filein,stringsAsFactors = F)
  attr(df,"filename") <- filein
  return(df)
}





#' Read in SEA data an ELG event file
#'
#' SEA event files contain output from a number of instruments
#' including GPS, flow-through, chirp, etc.
#'
#' @param filein .elg file to be read in
#' @param clean logical used to determine whether certain fields are
#' despiked and times when system is backflooded can be removed.
#' @keywords
#' @export
#' @examples
#' readSEAelg()
#'
readSEAelg <- function(filein,clean=T) {

  # Read in elg file using readr package (quicker than base)
  # TODO: Check headers of elgs from both ships to see if we can prespecify column types
  df <- read_csv(filein)

  # Reasign names that have dashes in them to be referenced more easily
  names(df) <- gsub("-",".",names(df))


  # Date and time parsing
  df$Time <- parse_time(df$Time)
  df$Date <- parse_date(df$Date,format="%m/%d/%Y")
  df$Sys_dttm <- update(df$Date,hour=hour(df$Time),minute=minute(df$Time),second=second(df$Time))

  if(hasName(df,"GPS.nav.time")) {
    df$GPS.nav.time <- parse_time(df$GPS.nav.time,format="%H%M%S")
    difft <- df$GPS.nav.time - df$Time
    rmdifft <- runmed(difft,11)
    difft[abs(difft)>abs(rmdifft)] <- rmdifft[abs(difft)>abs(rmdifft)]
    df$GPS.nav.dttm <- df$Sys_dttm+difft
  }
  if(hasName(df,"GPS.lab.time")) {
    df$GPS.lab.time <- parse_time(df$GPS.lab.time,format="%H%M%S")
    difft <- df$GPS.lab.time - df$Time
    rmdifft <- runmed(difft,11)
    difft[abs(difft)>abs(rmdifft)] <- rmdifft[abs(difft)>abs(rmdifft)]
    df$GPS.lab.dttm <- df$Sys_dttm+difft
  }

  # Location
  df$GPS.lab.Lat <- parse_lat(df$GPS.lab.Lat)
  df$GPS.lab.Lon <- parse_lon(df$GPS.lab.Lon)
  df$GPS.nav.Lat <- parse_lat(df$GPS.nav.Lat)
  df$GPS.nav.Lon <- parse_lon(df$GPS.nav.Lon)

  nlab <- length(which(is.na(df$GPS.lab.Lon)))
  nnav <- length(which(is.na(df$GPS.nav.Lon)))

  if(nlab >= nnav) {
    df$lon <- df$GPS.lab.Lon
    df$lat <- df$GPS.lab.Lat
  } else {
    df$lon <- df$GPS.nav.Lon
    df$lat <- df$GPS.nav.Lat
  }

  if(clean==T) {
    df$Tsal.temp <- despike(df$Tsal.temp)
    df$Tsal.sal <- despike(df$Tsal.sal)
    naloc <- df$Tsal.sal<20
    df[naloc,3:dim(df)[2]] = NA
    if(diff(range(df$lon,na.rm=T))>180) {
      df$lon[df$lon<0&!is.na(df$lon)] <- df$lon[df$lon<0&!is.na(df$lon)]+360;
    }
  }

  return(df)

}




#' Parse lon from elg file
#'
#' @param lonin lonto process
#' @keywords
#' @export
#' @examples
#' parse_lon()
#'
parse_lon <- function(lonin) {

  # TODO: replace substring with tidyverse equivelent
  len <- median(nchar(lonin),na.rm=T)
  exp <- "[0-9]{5}.[0-9]{1-4}"
  hemi <- ((substring(as.character(lonin),len,len)=='E')+0)*2-1
  lon1 <- as.character(lonin)
  lon1[nchar(lon1)!=len] <- NA
  lon1 <- substring(lon1,1,len-1)
  lon1[!1:length(lon1)%in%grep(exp,lon1)] <- NA
  lon <- hemi*as.numeric(substring(lon1,1,3))+hemi*as.numeric(substring(lon1,4,20))/60

  return(lon)

}


#' Parse lat from elg file
#'
#' @param latin lat to process
#' @keywords
#' @export
#' @examples
#' parse_lat()
#'
parse_lat <- function(latin) {

  # Replace substring with tidyverse equivelent
  len <- median(nchar(latin),na.rm=T)
  exp <- "[0-9]{4}.[0-9]{1-4}"
  hemi = ((substring(as.character(latin),len,len)=='N')+0)*2-1
  lat1 <- as.character(latin)
  lat1[nchar(lat1)!=len] <- NA
  lat1 <- substring(lat1,1,len-1)
  lat1[!1:length(lat1)%in%grep(exp,lat1)] <- NA
  lat <- hemi*as.numeric(substring(lat1,1,2))+hemi*as.numeric(substring(lat1,3,20))/60

  return(lat)

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
