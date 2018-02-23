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
  if(length(files)==0) {
    files <- list.files(path=foldin, pattern="^ADCP.*60.txt$")
  }
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
    nrows <- which(is.na(df[5]))[1]-1
    if(!is.na(nrows)) {
      if(nrows==0) {
        nrows <- which(is.na(df[5]))[2]-1
      }
      if(!is.na(nrows)) {
        df <- df[1:nrows,]
      }
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





#' Generic reader for SEA excel data sheets
#'
#' Produces a standardized raw output of a read excel file. It wont standardize column names, but it will ensure that data blocks are good.
#' Gets called from datasheet parsers (read_hourly, read_neuston, etc.)
#' Shouldn't ever need to call this directly, but could do to quick-check data
#'
#' @param filein xls file input
#'
#' @return
#' @export
#'
#' @examples
#' read_datasheet()
read_datasheet <- function(filein,sheet=1,n_max=100,range=readxl::cell_cols(1:100)) {

  df<-readxl::read_excel(filein,sheet=sheet,col_types = "text",n_max=n_max,range=range)

  # First heading Line gets read in as column names
  colnames(df) <- str_replace_all(colnames(df),"[-\\+\\ ]",".")

  # Remove all lines that dont have an entry in the first slot
  df <- dplyr::filter(df,!is.na(df[[1]]))

#   # Remove all empty columns - this shouldn't be necessary now that range is specified in read_excel
#   if(ncol(df)>200) {
#     df <- dplyr::select(df,1:200)
#   }
#
#   df <- dplyr::select(df,which(unlist(map(map(df,is.na),sum)) != nrow(df)))

  # First, check to see if there is a second heading line
  # The following checks to see if there are no purely numeric values
  # This is the test of an additional header line
  # NB: also works for blank line
  if(is_empty(str_which(df[1,],"^[0-9\\.]+$"))) {

    goodcols <- which(!(is.na(df[1,]) & str_detect(colnames(df),"^X__[0-9]+$")))
    df <- dplyr::select(df,goodcols)

    # Can do better than this, but currently this does the job of finding and condensing the two header lines
    topline <- which(!str_detect(names(df),"^X__[0-9]+$"))
    diffline <- diff(c(topline,ncol(df)))-1
    tline <- rep("",ncol(df))
    for (i in 1:length(topline)) {
      tline[topline[i]+(0:diffline[i])] <- str_c(colnames(df)[topline[i]],".")
    }

    new_heads <- str_c(tline,str_replace_all(df[1,],"[^0-9A-Za-z]","."))

    df <- df[2:nrow(df),]
    colnames(df) <- new_heads
  }

  # remove trailing values for stupid excel sheets that have stupid trailing vals in first column
  # currently done by looking for character lengths that are less than 3, and dont contain either a [A-Z] or [0-9]

  keepers <- str_length(df[[1]])>2
  df <- dplyr::filter(df,keepers)

}


#' Read in a neuston data sheet
#'
#' @param filein
#'
#' @return
#' @export
#'
#' @examples
read_neuston <- function(filein) {

  # read in the neuston data sheet in two two tibbles
  df1 <- read_datasheet(filein)
  df2 <- read_datasheet(filein,sheet=2)

  # check to see if data froms have the same number of row
  if(nrow(df1)!=nrow(df2)) {
    warning("Sheets in neuston excel document have different number of rows. Matching from row 1.")
  }

  # Need to make this robust - what if there were no matching column names?
  # df <- inner_join(df1,df2)
  df <- bind_cols(df1,df2)

  # ADD COMPLETE LIST OF ARGUMENTS (INCORPORATE NAMES?)

  args <- tribble(
    ~df,~name,~regex,~parse_fun,
    df,"station","^station",parse_character,
    df,"date","^date",parse_integer,
    df,"time_in","^time.*in",parse_double,
    df,"time_out","^time.*out",parse_double,
    df,"lon","londec",parse_double,
    df,"lat","latdec",parse_double,
    df,"temp","temp",parse_double,
    df,"sal","sal",parse_double,
    df,"fluor","fluor",parse_double,
    df,"moon_phase","moon.*phase",parse_double,
    df,"moon_mode",c("set.*risen","risen.*set"),parse_character,
    df,"cloud_cover","cloud.*cover",parse_double,
    df,"wind","wind.*cond",parse_character,
    df,"current","adcp",parse_character,
    df,"heading","heading",parse_double,
    df,"tow_dist",c("distance.*m","tow.*distance"),parse_double,
    df,"biomass","zoop.*bio",parse_double,
    df,"phyl_num","^phyl.*[^a-z]$",parse_integer,
    df,"phyl_vol","^phyl.*ml",parse_double,
    df,"lept_num","^lept.*[^a-z]$",parse_integer,
    df,"lept_vol","^lept.*ml",parse_double,
    df,"myct_num","^myct.*[^a-z]$",parse_integer,
    df,"myct_vol","^myct.*ml",parse_double,
    df,"ceph_num","^ceph.*[^a-z]$",parse_integer,
    df,"ceph_vol","^ceph.*ml",parse_double,
    df,"nekt_other_num","^other.*nekton.*[^a-z]$",parse_integer,
    df,"nekt_other_vol","^other.*nekt.*ml",parse_double,
    df,"nekt_total_num","^total.*nekton.*[^a-z]$",parse_integer,
    df,"nekt_total_vol","^total.*nekton.*ml",parse_double,
    df,"nekt_info","^types.*nekton",parse_character,
    df,"halo_num","^halo.*[^a-z]$",parse_integer,
    df,"gelat_num","^gelat.*[^a-z]$",parse_integer,
    df,"gelat_vol","^gelat.*ml",parse_double,
    df,"gelat_info","types.*gelat",parse_character,
    df,"sarg_natans_I","s.*natan.*.i.",parse_double,
    df,"sarg_natans_II","s.*natan.*.ii.",parse_double,
    df,"sarg_natans_VIII","s.*natan.*.viii.",parse_double,
    df,"sarg_fluitans_III","s.*flui.*.iii.",parse_double,
    df,"plas_pieces","^plastic.*pieces",parse_integer,
    df,"plas_pellets","^plastic.*pellet",parse_integer,
    df,"tar","^tar",parse_integer
  )

  namelist <- as_vector(select(args,name))

  # Work out how to pass format arguments or just post-process afterward

  output <- pmap(select(args,df,regex,parse_fun),parse_field)


  names(output) <- as_vector(select(args,name))
  output <- as.tibble(output)

  # parse the datetime field
  output$date <- as_date(output$date,origin="1900-1-1")
  local_in <- as_datetime(output$time_in*60*60*24)
  date(local_in) <- output$date
  local_out <- as_datetime(output$time_out*60*60*24)
  date(local_out) <- output$date
  difft <- local_out - local_in
  difft[difft>200] <- difft[difft>60]-dhours(24)
  local_out <- local_in + difft
  output <- add_column(output,dttm_in = local_in,dttm_out = local_out,.after=1)

  namelist <- append(namelist,c("dttm_in","dttm_out"),after = 1)
  namelist <- namelist[!str_detect(namelist,"^time_in$|^time_out$|^date$")]

  # further parse the wind and current fields



  # further parse the lat/lon fields

  return(output)

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
  nanloc <- is.na(df$Sys_dttm)
  dummyx <- 1:length(df$Sys_dttm)
  df$Sys_dttm <- as.POSIXct(approx(dummyx[!nanloc],df$Sys_dttm[!nanloc],dummyx)$y,origin="1970-1-1",tz="UTC")

  # Check if nav time field exists and make dttm if so
  if(hasName(df,"GPS.nav.time")) {
    df$GPS.nav.time <- parse_time(df$GPS.nav.time,format="%H%M%S")
    difft <- df$GPS.nav.time - df$Time
    nanloc <- is.na(difft)
    dummyx <- 1:length(difft)
    difft <- approx(dummyx[!nanloc],difft[!nanloc],dummyx)$y
    rmdifft <- runmed(difft,11)
    difft[abs(difft)>abs(rmdifft)] <- rmdifft[abs(difft)>abs(rmdifft)]
    df$GPS.nav.dttm <- df$Sys_dttm+difft
  } else {
    df$GPS.nav.time <- NA
  }

  # check is lab time exist and make dttm if so
  if(hasName(df,"GPS.lab.time")) {
    df$GPS.lab.time <- parse_time(df$GPS.lab.time,format="%H%M%S")
    difft <- df$GPS.lab.time - df$Time
    nanloc <- is.na(difft)
    dummyx <- 1:length(difft)
    difft <- approx(dummyx[!nanloc],difft[!nanloc],dummyx)$y
    rmdifft <- runmed(difft,11)
    difft[abs(difft)>abs(rmdifft)] <- rmdifft[abs(difft)>abs(rmdifft)]
    df$GPS.lab.dttm <- df$Sys_dttm+difft
  }

  # Location
  df$GPS.lab.Lat <- parse_lat(df$GPS.lab.Lat)
  df$GPS.lab.Lon <- parse_lon(df$GPS.lab.Lon)
  df$GPS.nav.Lat <- parse_lat(df$GPS.nav.Lat)
  df$GPS.nav.Lon <- parse_lon(df$GPS.nav.Lon)

  # TODO: Set nav GPS to be the default and fill with lab values as necessary

  nlab <- length(which(is.na(df$GPS.lab.Lon)))
  nnav <- length(which(is.na(df$GPS.nav.Lon)))

  if(nlab <= nnav) {
    df$lon <- df$GPS.lab.Lon
    df$lat <- df$GPS.lab.Lat
  } else {
    df$lon <- df$GPS.nav.Lon
    df$lat <- df$GPS.nav.Lat
  }

  if(clean==T) {
    df$Tsal.temp <- despike(df$Tsal.temp)
    df$Tsal.sal <- despike(df$Tsal.sal)
    naloc <- df$Tsal.sal<20 | is.na(df$Tsal.sal)
    df[naloc,3:dim(df)[2]] <- NA
    if(diff(range(df$lon,na.rm=T))>180) {
      df$lon[df$lon<0&!is.na(df$lon)] <- df$lon[df$lon<0&!is.na(df$lon)]+360;
    }
  }

  return(df)

}


#' Read in SEA data an ELG event file and return a well formatted output
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
#' read_elg()
#'
read_elg <- function(filein,forceGPS=c(NA,'nav','lab'),preCheck = T) {

  # TODO: Optimize code using pmap from purrr Package

  # Toggle a preCheck of the elg file for clearly bad lines and commas at end
  if(preCheck) {
    # check for bad lines by checking for number of commas
    liner <- read_lines(filein)
    numcom <- str_count(liner,"\\,")
    liner <- liner[numcom==numcom[1]]

    # clean and process end of line
    liner <- liner %>%
      str_replace("\\,$","") %>%    # remove the trailing comma on many ELG files
      str_replace("$","\\\n") %>%   # add new line to end of each line
      str_c(collapse="")           # collapse vector into single line for read_csv

    # Read in lines using readr package (quicker than base read.csv)
    df <- read_csv(liner,col_types = cols(.default=col_character()))

  } else {
    # If no precheck then just read in the file as is
    df <- read_csv(filein,col_types = cols(.default=col_character()))
  }

  # Reasign names that have dashes in them to be referenced more easily
  names(df) <- str_replace(names(df),"-",".")
  df_names <- str_to_lower(names(df))


# system date time info ---------------------------------------------------

  # Find and parse system date
  sys_date <- parse_elg_field(df,"System Date","date",parse_date,format="%m/%d/%Y")

  # Find and parse system time
  sys_time <- parse_elg_field(df,"System Time","^time",parse_time)

  # assign System date time (sys_dttm) based off system date and time read
  if(length(which(is.na(sys_date)))==length(sys_date)) {
    warning("No date data available, all sys_dttm values set to NA")
    sys_dttm <- as_datetime(rep(NA,nrow(df)))
  } else if (length(which(is.na(sys_time)))==length(sys_time)) {
    warning("No time data available, all sys_dttm values set to NA")
    sys_dttm <- as_datetime(rep(NA,nrow(df)))
  } else {
    sys_dttm <- update(sys_date,hour=hour(sys_time),minute=minute(sys_time),second=second(sys_time))
  }

# Lab GPS data ------------------------------------------------------------

  output <- parse_elg_gps(df,"lab")
  for (i in 1:length(output)) {
    assign(names(output)[i],output[[i]])
  }

# nav GPS data ------------------------------------------------------------

  output <- parse_elg_gps(df,"nav")
  for (i in 1:length(output)) {
    assign(names(output)[i],output[[i]])
  }

# Assign dttm to GPS as able -------------------------------------

  if(length(which(is.na(lab_time))) < length(lab_time)) {
    difft <- lab_time - sys_time
    goodi <- !is.na(difft)
    rmdifft <- runmed(difft,11)
    difft[abs(difft)>abs(rmdifft)&goodi] <- rmdifft[abs(difft)>abs(rmdifft)&goodi]
    lab_dttm <- sys_dttm+difft
  } else {
    lab_dttm <- rep(NA,nrow(df))
  }

  if(length(which(is.na(nav_time))) < length(nav_time)) {
    difft <- nav_time - sys_time
    goodi <- !is.na(difft)
    rmdifft <- runmed(difft,11)
    difft[abs(difft)>abs(rmdifft)&goodi] <- rmdifft[abs(difft)>abs(rmdifft)&goodi]
    nav_dttm <- sys_dttm+difft
  } else {
    nav_dttm <- parse_datetime(rep(NA,nrow(df)))
  }

# produce master GPS lon, lat and time as able -------------------------------------
  # use nav GPS as the default and revert to lab gps and sys time as required
  if(is.na(forceGPS)) {
    lon <- nav_lon
    lon[is.na(lon) & !is.na(lab_lon)] <- lab_lon[is.na(lon) & !is.na(lab_lon)]

    lat <- nav_lat
    lat[is.na(lat) & !is.na(lab_lat)] <- lab_lat[is.na(lat) & !is.na(lab_lat)]

    dttm <- nav_dttm
    dttm[is.na(dttm) & !is.na(lab_dttm)] <- lab_dttm[is.na(dttm) & !is.na(lab_dttm)]
  } else if (forceGPS == 'nav') {
    lon <- nav_lon
    lat <- nav_lat
    dttm <- nav_dttm
  } else if (forceGPS == 'lab') {
    lon <- lab_lon
    lat <- lab_lat
    dttm <- lab_dttm
  }

# Flowthrough data -------------------------------------------------

  # Find and parse temperature
  temp <- parse_elg_field(df,"Temperature","tsal.*temp",parse_double)
  temp[temp< -3|temp>50|is.na(temp)] <- NA # NB temperature bracket inserted here

  # Find and parse salinity
  sal <- parse_elg_field(df,"Salinity","tsal.*sal",parse_double)
  sal[sal< 0|sal>50|is.na(sal)] <- NA # NB salinity bracket inserted here

  # Find and parse fluoroesence
  fluor <- parse_elg_field(df,"Fluoroesence","^fluo.*invivo",parse_double)
  fluor_1min <- parse_elg_field(df,"Fluoroesence 1-min average","fluo.*chl.*1.*min",parse_double)
  fluor_60min <- parse_elg_field(df,"Fluoroesence 60-min average","^fluo.*chl.*60.*min",parse_double)

  # Find and parse CDOM
  CDOM <- parse_elg_field(df,"CDOM","cdom.*raw",parse_double)
  CDOM_1min <- parse_elg_field(df,"CDOM 1-min average","cdom.*1.*min",parse_double)
  CDOM_60min <- parse_elg_field(df,"CDOM 60-min average","cdom.*60.*min",parse_double)

  # Find and parse Transmissivity
  xmiss <- parse_elg_field(df,"Transmissivity",c("trans.*raw","xmiss.*raw"),parse_double)
  xmiss_1min <- parse_elg_field(df,"Transmissivity 1-min average",c("trans.*1\\.*min","xmiss.*1\\.*min"),parse_double)
  xmiss_60min <- parse_elg_field(df,"Transmissivity 60-min average",c("trans.*60.*min","xmiss.*60.*min"),parse_double)


# Wind --------------------------------------------------------------------

  # find and parse Wind Speed and Direction
  wind_sp <- parse_elg_field(df,"True Wind Speed","true.*wind.*sp",parse_double)
  wind_dir <- parse_elg_field(df,"True Wind Direction","true.*wind.*dir",parse_double)
  wind_sp_rel <- parse_elg_field(df,"Relative Wind Speed","^wind.*sp",parse_double)
  wind_dir_rel <- parse_elg_field(df,"Relative Wind Speed","^wind.*dir",parse_double)


# Heading, Pitch and Roll -------------------------------------------------

  heading <- parse_elg_field(df,"Heading",c("hdg","heading"),parse_double)
  heading[heading>360|is.na(heading)] <- NA

  pitch <- parse_elg_field(df,"Pitch","pitch",parse_double)
  pitch[abs(pitch)>90|is.na(pitch)] <- NA

  roll <- parse_elg_field(df,"Roll","roll",parse_double)
  roll[abs(roll)>90|is.na(pitch)] <- NA


# Depth -------------------------------------------------------------------

  depth <- parse_elg_field(df,"CHIRP Depth",c("depth","dbt"),parse_double)


# LCI90 -------------------------------------------------------------------

  wire_payout <- parse_elg_field(df,"Wire Payout","lci90.*payout",parse_double)
  wire_tension <- parse_elg_field(df,"Wire Tension","lci90.*tension",parse_double)
  wire_speed <- parse_elg_field(df,"Wire Speed","lci90.*spd",parse_double)


# Combine into output tibble ----------------------------------------------

  # TODO: Ensure that all fields fit their class before export. Or ensure they are made right.
  output <- tibble(dttm,lon,lat,sys_dttm,sys_date,sys_time,
                   nav_dttm,nav_time,nav_lon,nav_lat,nav_sog,nav_cog,nav_quality,
                   lab_dttm,lab_time,lab_lon,lab_lat,lab_sog,lab_cog,lab_quality,
                   temp,sal,
                   fluor,fluor_1min,fluor_60min,
                   CDOM,CDOM_1min,CDOM_60min,
                   xmiss,xmiss_1min,xmiss_60min,
                   wind_sp,wind_dir,wind_sp_rel,wind_dir_rel,
                   heading,roll,pitch,depth,
                   wire_speed,wire_payout,wire_tension)


}



# ELG Parse functions -----------------------------------------------------


#' Generic field parser - can be used for all elg and excel parsing
#'
#' Returns tibble with found column parsed and formatted
#'
#' @param df
#' @param regex
#' @param name
#' @param parse_fun
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
parse_field <- function(df,regex,parse_fun,...) {

  df_names <- str_to_lower(names(df))

  # Find and parse field
  for (i in 1:length(regex)) {
    ii <- str_which(df_names,regex[i])
    if(length(ii)>0) break
  }

  if (length(ii)>1) {
    # warning(paste(name,"found in multiple slots:", paste0(ii,collapse="; "), ". By default, using data from slot", ii[1]))
    ii <- ii[1]
  }
  if (length(ii)==0) {
    # warning(paste(name,"not found in ELG file. Setting all values to NA"))
    output <- parse_fun(df[[ii]],...)
    # names(output) <- name
    # output <- list(parse_fun(rep(NA,nrow(df)),...))
    # names(output) <- name
    # df <- bind_cols(df,output)
  } else {
    # message(paste("Reading", name, "data from slot", ii))
    output <- parse_fun(df[[ii]],...)
    # names(output) <- name
    # names(df)[ii] <- name
    # df[[ii]] <- parse_fun(df[[ii]],...)
  }

  return(output)


}



#' Generic parser for fields contained in araw charactor tibble
#'
#' Data read in to character tibble by read_csv(...,col_types = cols(.default=col_character()))).
#' Takes parsing function as input so can differentiate between different column types.
#' Takes tibble and returns vector
#'
#' @param df character tibble
#' @param field name of field to parse (passed to warning and message functions for readability).
#' @param regex regex for finding the appropriate field. Can be vector of searches with earlier values prioritized.
#' @param parse_fun the parsing function to use in creating the new vector
#' @param ... additional parameters passed to parse_fun (i.e. format = "" for parse_date)
#'
#' @return
#' @export
#'
#' @examples
#' parse_elg_field()
parse_elg_field <- function(df,field,regex,parse_fun,...) {

  df_names <- str_to_lower(names(df))

  # Find and parse field
  for (i in 1:length(regex)) {
    ii <- grep(regex[i],df_names)
    if(length(ii)>0) break
  }

  if (length(ii)>1) {
    warning_mult(field,ii)
    ii <- ii[1]
  }
  if (length(ii)==0) {
    warning_empty(field)
    output <- parse_fun(rep(NA,nrow(df)),...)
    # output <- list(parse_fun(rep(NA,nrow(df)),...))
    # names(output) <- name
    # df <- bind_cols(df,output)
  } else {
    message_read(field,ii)
    # names(df[ii]) <- name
    # df[[ii]] <- parse_fun(df[[ii]],...)
    output <- parse_fun(df[[ii]],...)
  }



}

#' Parse GPS fields from ELG file
#'
#' Takes in a raw character tibble data frame and parses the 6 potential GPS fields
#' Takes tibble, returns vector
#'
#' @param df tibble character data frame input
#' @param source choose which GPS source to parse ("lab","nav")
#'
#' @return
#' @export
#'
#' @examples
#' parse_gps()
parse_elg_gps <- function(df,source){
  df_names <- str_to_lower(names(df))

  output <- NULL

  fieldnames <- c("time","lon","lat","sog","cog","quality")
  for (i in 1:6) {
    field <- paste(str_to_title(source),"GPS",str_to_title(fieldnames[i]))
    regexp <- paste("gps",source,fieldnames[i],sep=".")
    outfield <- paste0(source,"_", fieldnames[i])

    ii <- grep(regexp,df_names)
    if (length(ii)>1) {
      warning_mult(field,ii)
      ii <- ii[1]
    }
    if(length(ii)==0) {
      warning_empty(field)
      # TODO: Ensure that correct class is passed on to each empty field here
      assign(outfield,rep(NA,nrow(df)))
    } else {
      message_read(field,ii)
      if (i == 1) {
        assign(outfield,parse_time(str_extract(df[[ii]],"[0-9]{6}"),format="%H%M%S"))
      } else if (i == 2) {
        assign(outfield,parse_lon(df[[ii]]))
      } else if (i == 3) {
        assign(outfield,parse_lat(df[[ii]]))
      } else if (i == 4 | i == 5) {
        assign(outfield,parse_double(df[[ii]]))
      } else if (i == 6) {
        assign(outfield,parse_integer(df[[ii]]))
      }
    }
    output[[i]] <- get(outfield)
    names(output)[i] <- outfield
  }

  return(output)
}
# TODO: Work out if you can change the function called to parse the data on the fly
# Should be able to generate generic parsers for all fields
# See parse_elg_field - need to find a way to impliment for parse_gps


# warning/message functions -------------------------------------------------------

# NB: All these functions should become obsolete when parsing of elg fields is optimized


#' Produces warning for an empty field
#'
#' Called during parse_elg_* functions if no match to the input regex is found
#'
#' @param field The proper name of the field - gets passed to output string to better read
#'
#' @return
#' @export
#'
#' @examples
#' warning_empty("Temperature")
warning_empty <- function(field) {
  warning(paste(field,"not found in ELG file. Setting all values to NA"))
}

#' Produces warning for multiple name matches
#'
#' Called during parse_elg_* functions if multiple matches to the input regex are found
#' NB: by default the first value is selected.
#'
#' @param field  The proper name of the field - gets passed to output string to better read
#' @param ii matched field names
#'
#' @return
#' @export
#'
#' @examples
#' warning_mult("Temperature,c(2,23))
warning_mult <- function(field,ii) {
  warning(paste(field,"found in multiple slots:", paste0(ii,collapse="; "), ". By default, using data from slot", ii[1]))
}

#' Prints message that states which slot is being read
#'
#' Called during parse_elg_* functions when field is being parsed
#'
#' @param field The proper name of the field - gets passed to output string to better read
#' @param ii slot used for the parsing
#'
#' @return
#' @export
#'
#' @examples
#' message_read("Temperature",13)
message_read <- function(field,ii) {
  message(paste("Reading", field, "data from slot", ii))
}


#' Cut out bad lines of elg file based on bad nav gps signals
#'
#' SEA event files contain output from a number of instruments
#' including GPS, flow-through, chirp, etc.
#'
#' @param filein .elg filepath to be read in
#' @param latstr regexp string to use to search for
#' @param fileout a specified output filepath.
#' If not specified, "_clean" is appended to name of input file and saved in the same directory.
#' @keywords
#' @export
#' @examples
#' clean_bad_elg()
#'
clean_bad_elg <- function(filein,latstr="[0-9]{4}\\.[0-9]{4}[NS]{1}",lonstr="[0-9]{5}\\.[0-9]{4}[EW]{1}",fileout=NULL) {

  # read elg data as charactor tibble
  df <- read_csv(filein,col_types = cols(.default = col_character()))
  names(df) <- gsub("-",".",names(df))

  # find all the correctly and incorrectly formatted latitude nav values using latstr


  keep1 <- 1:length(df$GPS.nav.Lat) %in% grep(latstr,df$GPS.nav.Lat)
  keep2 <- 1:length(df$GPS.nav.Lon) %in% grep(lonstr,df$GPS.nav.Lon)
  keep3 <- 1:length(df$GPS.nav.quality) %in% grep("^1$",df$GPS.nav.quality)

  badi <- which(!keep1|!keep2|!keep3)
  goodi <- which(keep1&keep2&keep3)

  # read in the raw lines from the elg file
  df_raw <- read_lines(filein)

  # only keep those lines which are good (plus the header line)
  df_raw_clean <- df_raw[append(1,goodi+1)]

  # create output filename if not specified in function call
  if(is.null(fileout)) {
    fileout <- str_replace(filein,".elg","_clean.elg")
  }

  # write data to file
  write_lines(df_raw_clean,fileout)

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
