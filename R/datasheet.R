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
  colnames(df) <- stringr::str_replace_all(colnames(df),"[-\\+\\ ]",".")

  # Remove all lines that dont have an entry in the first slot
  df <- dplyr::filter(df,!is.na(df[[1]]))


  # First, check to see if there is a second heading line
  # The following checks to see if there are no purely numeric values
  # This is the test of an additional header line
  # NB: also works for blank line
  if(length(stringr::str_which(df[1, ], "^[0-9\\.]+$")) == 0) {

    goodcols <- which(!(is.na(df[1,]) & stringr::str_detect(colnames(df),"^X__[0-9]+$")))
    df <- dplyr::select(df,goodcols)

    # Can do better than this, but currently this does the job of finding and condensing the two header lines
    topline <- which(!stringr::str_detect(names(df),"^X__[0-9]+$"))
    diffline <- diff(c(topline,ncol(df)))-1
    tline <- rep("",ncol(df))
    for (i in 1:length(topline)) {
      tline[topline[i]+(0:diffline[i])] <- stringr::str_c(colnames(df)[topline[i]],".")
    }

    new_heads <- stringr::str_c(tline,stringr::str_replace_all(df[1,],"[^0-9A-Za-z]","."))

    df <- df[2:nrow(df),]
    colnames(df) <- new_heads
  }

  # remove trailing values for stupid excel sheets that have stupid trailing vals in first column
  # currently done by looking for character lengths that are less than 3, and dont contain either a [A-Z] or [0-9]

  keepers <- stringr::str_length(df[[1]])>2
  if (sum(keepers,na.rm=T) > 0) {
    df <- dplyr::filter(df,keepers)
  } else {
    df <- NULL
  }


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

  if(nrow(df1) == 1 & is.null(df2)) {
    return(NULL)
  }

  # check to see if data froms have the same number of row
  if(nrow(df1)!=nrow(df2)) {
    warning("Sheets in neuston excel document have different number of rows. Matching from row 1.")
  }

  # Need to make this robust - what if there were no matching column names?
  # df <- inner_join(df1,df2)
  df <- dplyr::bind_cols(df1,df2)

  # ADD COMPLETE LIST OF ARGUMENTS (INCORPORATE NAMES?)

  args <- tibble::tribble(~name,~regex,~parse_fun,
                          "station","^station",readr::parse_character,
                          "date","^date",readr::parse_integer,
                          "time_in","^time.*in",readr::parse_double,
                          "time_out","^time.*out",readr::parse_double,
                          "lon","londec",readr::parse_double,
                          "lat","latdec",readr::parse_double,
                          "temp","temp",readr::parse_double,
                          "sal","sal",readr::parse_double,
                          "fluor","fluor",readr::parse_double,
                          "moon_phase","moon.*phase",readr::parse_double,
                          "moon_mode",c("set.*risen","risen.*set"),readr::parse_character,
                          "cloud_cover","cloud.*cover",readr::parse_double,
                          "wind","wind.*cond",readr::parse_character,
                          "current","adcp",readr::parse_character,
                          "heading","heading",readr::parse_double,
                          "tow_dist",c("distance.*m","tow.*distance"),readr::parse_double,
                          "biomass","zoop.*bio",readr::parse_double,
                          "phyl_num","^phyl.*[^a-z]$",readr::parse_integer,
                          "phyl_vol","^phyl.*ml",readr::parse_double,
                          "lept_num","^lept.*[^a-z]$",readr::parse_integer,
                          "lept_vol","^lept.*ml",readr::parse_double,
                          "myct_num","^myct.*[^a-z]$",readr::parse_integer,
                          "myct_vol","^myct.*ml",readr::parse_double,
                          "ceph_num","^ceph.*[^a-z]$",readr::parse_integer,
                          "ceph_vol","^ceph.*ml",readr::parse_double,
                          "nekt_other_num","^other.*nekton.*[^a-z]$",readr::parse_integer,
                          "nekt_other_vol","^other.*nekt.*ml",readr::parse_double,
                          "nekt_total_num","^total.*nekton.*[^a-z]$",readr::parse_integer,
                          "nekt_total_vol","^total.*nekton.*ml",readr::parse_double,
                          "nekt_info","^types.*nekton",readr::parse_character,
                          "halo_num","^halo.*[^a-z]$",readr::parse_integer,
                          "gelat_num","^gelat.*[^a-z]$",readr::parse_integer,
                          "gelat_vol","^gelat.*ml",readr::parse_double,
                          "gelat_info","types.*gelat",readr::parse_character,
                          "sarg_natans_I","s.*natan.*.i.",readr::parse_double,
                          "sarg_natans_II","s.*natan.*.ii.",readr::parse_double,
                          "sarg_natans_VIII","s.*natan.*.viii.",readr::parse_double,
                          "sarg_fluitans_III","s.*flui.*.iii.",readr::parse_double,
                          "plas_pieces","^plastic.*pieces",readr::parse_integer,
                          "plas_pellets","^plastic.*pellet",readr::parse_integer,
                          "tar","^tar",readr::parse_integer
                          )
  args_in <- tibble::as_tibble(list(df=list(df),regex=args$regex,parse_fun=args$parse_fun))
  namelist <- purrr::as_vector(dplyr::select(args,name))

  # Work out how to pass format arguments or just post-process afterward

  output <- purrr::pmap(args_in,parse_field)

  names(output) <- purrr::as_vector(dplyr::select(args,name))
  output <- tibble::as.tibble(output)

  # parse the datetime field
  output$date <- lubridate::as_date(output$date,origin="1900-1-1")
  local_in <- lubridate::as_datetime(output$time_in*60*60*24)
  lubridate::date(local_in) <- output$date
  local_out <- lubridate::as_datetime(output$time_out*60*60*24)
  lubridate::date(local_out) <- output$date
  difft <- local_out - local_in
  difft[difft>200] <- difft[difft>60]-lubridate::dhours(24)
  local_out <- local_in + difft
  output <- tibble::add_column(output,dttm_in = local_in,dttm_out = local_out,.after=1)

  namelist <- append(namelist,c("dttm_in","dttm_out"),after = 1)
  namelist <- namelist[!stringr::str_detect(namelist,"^time_in$|^time_out$|^date$")]

  # further parse the wind and current fields



  # further parse the lat/lon fields

  return(output)

}


#' Read Surface Sample Datasheet
#'
#' Function produces well formatted data from reading in a SEA surfsamp excel datasheet.
#' These datasheets contain extracted chemical and auxilary data form surface samples
#' collected on the SEA vessels.
#'
#' @param filein
#'
#' @return
#' @export
#'
#' @examples
read_surfsamp <- function(filein) {

  # read in the surface station data sheet
  df <- read_datasheet(filein)

  if(is.null(df)) {
    warning("datasheet empty - continuing")
    return(df)
  } else {

     # ADD COMPLETE LIST OF ARGUMENTS (INCORPORATE NAMES?)

    args <- tibble::tribble(~name,~regex,~parse_fun,
                            "station","^station",readr::parse_character,
                            "date","^date",readr::parse_integer,
                            "time","^time",readr::parse_double,
                            "time_utc","utc.*time",readr::parse_double,
                            "lon","londec",readr::parse_double,
                            "lat","latdec",readr::parse_double,
                            "temp","temp",readr::parse_double,
                            "sal","sal",readr::parse_double,
                            "fluor","fluor.*chl",readr::parse_double,
                            "chla","chl.*a.*g",readr::parse_double,
                            "chla_vol","chl.*a.*vol",readr::parse_double,
                            "po4","po4",readr::parse_double,
                            "no3","no3",readr::parse_double,
                            "sio2","sio2",readr::parse_double,
                            "o2","^o2",readr::parse_double,
                            "pH","ph",readr::parse_double,
                            "alk","alk",readr::parse_double,
                            "m_plastics","po4",readr::parse_double,
                            "notes","notes",readr::parse_character
                            )


    output <- parse_datasheet(df,args)

    # parse the datetime field
    output$date <- lubridate::as_date(output$date,origin="1900-1-1")
    local <- lubridate::as_datetime(output$time*60*60*24)
    lubridate::date(local) <- output$date

    df <- tibble::add_column(output,dttm_local = local,.after=1)



  }

  return(df)
}




#' Read a hydrocast datasheet
#'
#' Reads in formatted data from a SEA hydrowork excel datasheet. These datasheets record
#' bottle and auxilary data from deployment of a CTD carousel.
#'
#' @param filein the hydrowork datasheet
#'
#' @return
#' @export
#'
#' @examples
read_hydrocast <- function(filein) {
  # read in the surface station data sheet
  df <- read_datasheet(filein)

  if(is.null(df)) {
    return(NULL)
  } else {
    # ADD COMPLETE LIST OF ARGUMENTS (INCORPORATE NAMES?)

    args <- tibble::tribble(~name,~regex,~parse_fun,
                            "station","^station",readr::parse_character,
                            "date","^date",readr::parse_integer,
                            "time",c("^start","^time"),readr::parse_double,
                            "lon","londec",readr::parse_double,
                            "lat","latdec",readr::parse_double,
                            "temp_surf","temp",readr::parse_double,
                            "sal_surf","sal",readr::parse_double,
                            "fluor_surf",c("fluor.*chl","chl.*fluor"),readr::parse_double,
                            "bottle","bottle",readr::parse_character,
                            "z","z.*corr",readr::parse_double,
                            "temp","temp.*deg",readr::parse_double,
                            "sal","salinity.*psu",readr::parse_double,
                            "density","density",readr::parse_double,
                            "chla","chl.*a.*g",readr::parse_double,
                            "po4","po4",readr::parse_double,
                            "no3","nitrate",readr::parse_double,
                            "sio2","sio2",readr::parse_double,
                            "o2_sens","^o2.*sea",readr::parse_double,
                            "o2_wink","^o2.*wink",readr::parse_double,
                            "pH","ph",readr::parse_double,
                            "alk","^total.*alk",readr::parse_double,
                            "notes","notes",readr::parse_character
    )

    output <- parse_datasheet(df,args)

    output$date <- lubridate::as_date(output$date,origin="1900-1-1")
    local <- lubridate::as_datetime(output$time*60*60*24)
    lubridate::date(local) <- output$date
    df <- tibble::add_column(output,dttm = local,.after=1)
  }

}


#' Read in hourly datasheet
#'
#' Function produces well formatted data from reading in a SEA hourlywork excel datasheet.
#' This is houly resolution cruise track and sensor data.
#'
#' @param filein the hourly datasheet filepath
#'
#' @return
#' @export
#'
#' @examples
read_hourly <- function(filein) {

  # read in the surface station data sheet
  df <- read_datasheet(filein)

  if(is.null(df)) {
    return(NULL)
  } else {
    # ADD COMPLETE LIST OF ARGUMENTS (INCORPORATE NAMES?)

    args <- tibble::tribble(~name,~regex,~parse_fun,
                            "date","gmt.*date",readr::parse_integer,
                            "time","gmt.*time",readr::parse_double,
                            "lon","londec",readr::parse_double,
                            "lat","latdec",readr::parse_double,
                            "temp","^temp",readr::parse_double,
                            "sal","^sal",readr::parse_double,
                            "fluor","fluor",readr::parse_double,
                            "cdom_1min","cdom",readr::parse_double,
                            "xmiss_1min",'xmiss',readr::parse_double,
                            "depth_bot",'bottom.*depth',readr::parse_double,
                            "wind_sp",'wind.*speed',readr::parse_double,
                            "wind_dir","wind.*direc",readr::parse_double
                            )

    output <- parse_datasheet(df,args)

    output$date <- lubridate::as_date(output$date,origin="1900-1-1")
    local <- lubridate::as_datetime(output$time*60*60*24)
    lubridate::date(local) <- output$date
    df <- tibble::add_column(output,dttm = local,.after=0)
  }
}
