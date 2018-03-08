# Series of functions that work to read in an event data file from SEA's archive

#' Read in SEA data an ELG event file and return a well formatted output
#'
#' SEA event files contain output from a number of instruments
#' including GPS, flow-through, chirp, etc.
#'
#' @param filein .elg file to be read in
#' @param forceGPS option to force the longitude, latitude and time to come from one
#'  or other of the GPS feeds
#' @param preCheck logical to do an initial check of lines to remove any standard issues
#' @param skip number of lines to skip below header
#' @keywords
#' @export
#' @examples
#' read_elg()
#'
read_elg <- function(filein,forceGPS=NULL,preCheck = T,skip=0) {

  # TODO: Optimize code using pmap from purrr Package

  if(skip>0) {
    col_names <- names(read_csv(filein, n_max = 0))
  } else {
    col_names=T
  }

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
    df <- read_csv(liner,col_types = cols(.default=col_character()),skip=skip,col_names = col_names)

  } else {

    # If no precheck then just read in the file as is
    df <- read_csv(filein,col_types = cols(.default=col_character()),skip=skip,col_names = col_names)

  }

  # Reasign names that have dashes in them to be referenced more easily
  names(df) <- str_replace_all(names(df),"-",".")

  # Construct the
  args <- tibble::tribble(~name,~regex,~parse_fun,
                          "sys_date","date",mdy,
                          "sys_time","^time",parse_time,
                          "nav_time","gps.*nav.*time",parse_character,
                          "nav_lon","gps.*nav.*lon",parse_lon,
                          "nav_lat","gps.*nav.*lat",parse_lat,
                          "nav_sog","gps.*nav.*sog",parse_double,
                          "nav_cog","gps.*nav.*cog",parse_double,
                          "nav_quality","gps.*nav.*quality",parse_integer,
                          "lab_time","gps.*lab.*time",parse_character,
                          "lab_lon","gps.*lab.*lon",parse_lon,
                          "lab_lat","gps.*lab.*lat",parse_lat,
                          "lab_sog","gps.*lab.*sog",parse_double,
                          "lab_cog","gps.*lab.*cog",parse_double,
                          "lab_quality","gps.*lab.*quality",parse_integer,
                          "temp","tsal.*temp",parse_double,
                          "sal","tsal.*sal",parse_double,
                          "fluor","^fluo.*invivo",parse_double,
                          "fluor_1min","fluo.*chl.*1.*min",parse_double,
                          "fluor_60min","^fluo.*chl.*60.*min",parse_double,
                          "CDOM","cdom.*raw",parse_double,
                          "CDOM_1min","cdom.*1.*min",parse_double,
                          "CDOM_60min","cdom.*60.*min",parse_double,
                          "xmiss",c("trans.*raw","xmiss.*raw","xmiss.*[^m]"),parse_double,
                          "xmiss_1min",c("trans.*1\\.*min","xmiss.*1\\.*min"),parse_double,
                          "xmiss_60min",c("trans.*60.*min","xmiss.*60.*min"),parse_double,
                          "wind_sp","true.*wind.*sp",parse_double,
                          "wind_dir","true.*wind.*dir",parse_double,
                          "wind_sp_rel","^wind.*sp",parse_double,
                          "wind_dir_rel","^wind.*dir",parse_double,
                          "heading",c("hdg","heading"),parse_double,
                          "pitch","pitch",parse_double,
                          "roll","roll",parse_double,
                          "depth",c("depth","dbt"),parse_double,
                          "wire_payout","lci90.*payout",parse_double,
                          "wire_tension","lci90.*tension",parse_double,
                          "wire_speed","lci90.*spd",parse_double
                          )

  args_in <- as_tibble(list(df=list(df),regex=args$regex,parse_fun=args$parse_fun))
  namelist <- purrr::as_vector(dplyr::select(args,name))

  # Work out how to pass format arguments or just post-process afterward

  # output <- purrr::pmap(dplyr::select(args,df,regex,parse_fun),parse_field)
  output <- purrr::pmap(args_in,parse_field)

  names(output) <- namelist
  df <- tibble::as.tibble(output)

  # additional parsing for some elements
  df$nav_time <- parse_time(str_extract(df$nav_time,"[0-9]{6}"),format="%H%M%S")
  df$lab_time <- parse_time(str_extract(df$lab_time,"[0-9]{6}"),format="%H%M%S")
  df$sys_dttm <- update(df$sys_date,hour=hour(df$sys_time),minute=minute(df$sys_time),second=second(df$sys_time))

  # Make datetimes from GPS using the system datetime
  # TODO: functionize the following
  if(length(which(is.na(df$lab_time))) < length(df$lab_time)) {
    difft <- df$lab_time - df$sys_time
    goodi <- !is.na(difft)
    dayoffi <- difft < -8000
    k <- ceiling(as.numeric(max(difft[abs(difft)<2*60*60],na.rm=T)/20))
    if(!k%%2) {
      k <- k+1
    }
    rmdifft <- runmed(difft,k)
    difft[dayoffi & goodi] <- rmdifft[dayoffi & goodi]
    df <- mutate(df,lab_dttm = sys_dttm+difft)
  } else {
    df <- mutate(df,lab_dttm = parse_datetime(rep(NA,nrow(df))))
  }

  # largely repeated from above, but for NAV GPS
  if(length(which(is.na(df$nav_time))) < length(df$nav_time)) {
    difft <- df$nav_time - df$sys_time
    goodi <- !is.na(difft)
    dayoffi <- difft < -8000
    k <- ceiling(as.numeric(max(difft[abs(difft)<2*60*60],na.rm=T)/20))
    if(!k%%2) {
      k <- k+1
    }
    rmdifft <- runmed(difft,k)
    difft[dayoffi & goodi] <- rmdifft[dayoffi & goodi]
    df <- mutate(df,nav_dttm = sys_dttm+difft)
  } else {
    df <- mutate(df,nav_dttm = parse_datetime(rep(NA,nrow(df))))
  }

  # choose master datetime
  # use nav GPS as the default and revert to lab gps and sys time as required
  if(is.null(forceGPS)) {
    lon <- df$nav_lon
    lon[is.na(lon) & !is.na(df$lab_lon)] <- df$lab_lon[is.na(lon) & !is.na(df$lab_lon)]
    lat <- df$nav_lat
    lat[is.na(lat) & !is.na(df$lab_lat)] <- df$lab_lat[is.na(lat) & !is.na(df$lab_lat)]
    dttm <- df$nav_dttm
    dttm[is.na(dttm) & !is.na(df$lab_dttm)] <- df$lab_dttm[is.na(dttm) & !is.na(df$lab_dttm)]
  } else if (forceGPS == 'nav') {
    lon <- df$nav_lon
    lat <- df$nav_lat
    dttm <- df$nav_dttm
  } else if (forceGPS == 'lab') {
    lon <- df$lab_lon
    lat <- df$lab_lat
    dttm <- df$lab_dttm
  }

  # add the chosen, lon, lat and dttm
  df <- mutate(df,lon=lon,lat=lat,dttm=dttm)

  # rearrange the columns into correct order
  df <- df[,c(42,40,41,37,1,2,39,3:8,38,9:36)]

}





#' Update ELG data
#'
#' Add to an existing elg tibble from an SEA Event File that is in the process of being recorded.
#'
#' @param df current tibble of ELG data
#' @param filein location of the *.elg file to update from
#'
#' @return
#' @export
#'
#' @examples
update_elg <- function(df,filein,preCheck=T) {

  # Find number of rows to skip
  nskip <- nrow(df)+1

  # read in the lines
  dfadd <- read_elg(filein,skip=nskip,preCheck=preCheck)

  df <- bind_rows(df,dfadd)
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





# ELG Parse functions -----------------------------------------------------

# Could make the following two functions into one




# Following code has now been replaced with parse_field

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





