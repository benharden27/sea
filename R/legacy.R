

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


