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
  if(is_empty(str_which(df[1,],"^[0-9\\.]+$"))) {

    goodcols <- which(!(is.na(df[1,]) & str_detect(colnames(df),"^X__[0-9]+$")))
    df <- dplyr::select(df,goodcols)

    # Can do better than this, but currently this does the job of finding and condensing the two header lines
    topline <- which(!stringr::str_detect(names(df),"^X__[0-9]+$"))
    diffline <- diff(c(topline,ncol(df)))-1
    tline <- rep("",ncol(df))
    for (i in 1:length(topline)) {
      tline[topline[i]+(0:diffline[i])] <- stringr::str_c(colnames(df)[topline[i]],".")
    }

    new_heads <- stringr::str_c(tline,str_replace_all(df[1,],"[^0-9A-Za-z]","."))

    df <- df[2:nrow(df),]
    colnames(df) <- new_heads
  }

  # remove trailing values for stupid excel sheets that have stupid trailing vals in first column
  # currently done by looking for character lengths that are less than 3, and dont contain either a [A-Z] or [0-9]

  keepers <- stringr::str_length(df[[1]])>2
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
  df <- dplyr::bind_cols(df1,df2)

  # ADD COMPLETE LIST OF ARGUMENTS (INCORPORATE NAMES?)

  args <- tibble::tribble(~df,~name,~regex,~parse_fun,
                          df,"station","^station",readr::parse_character,
                          df,"date","^date",readr::parse_integer,
                          df,"time_in","^time.*in",readr::parse_double,
                          df,"time_out","^time.*out",readr::parse_double,
                          df,"lon","londec",readr::parse_double,
                          df,"lat","latdec",readr::parse_double,
                          df,"temp","temp",readr::parse_double,
                          df,"sal","sal",readr::parse_double,
                          df,"fluor","fluor",readr::parse_double,
                          df,"moon_phase","moon.*phase",readr::parse_double,
                          df,"moon_mode",c("set.*risen","risen.*set"),readr::parse_character,
                          df,"cloud_cover","cloud.*cover",readr::parse_double,
                          df,"wind","wind.*cond",readr::parse_character,
                          df,"current","adcp",readr::parse_character,
                          df,"heading","heading",readr::parse_double,
                          df,"tow_dist",c("distance.*m","tow.*distance"),readr::parse_double,
                          df,"biomass","zoop.*bio",readr::parse_double,
                          df,"phyl_num","^phyl.*[^a-z]$",readr::parse_integer,
                          df,"phyl_vol","^phyl.*ml",readr::parse_double,
                          df,"lept_num","^lept.*[^a-z]$",readr::parse_integer,
                          df,"lept_vol","^lept.*ml",readr::parse_double,
                          df,"myct_num","^myct.*[^a-z]$",readr::parse_integer,
                          df,"myct_vol","^myct.*ml",readr::parse_double,
                          df,"ceph_num","^ceph.*[^a-z]$",readr::parse_integer,
                          df,"ceph_vol","^ceph.*ml",readr::parse_double,
                          df,"nekt_other_num","^other.*nekton.*[^a-z]$",readr::parse_integer,
                          df,"nekt_other_vol","^other.*nekt.*ml",readr::parse_double,
                          df,"nekt_total_num","^total.*nekton.*[^a-z]$",readr::parse_integer,
                          df,"nekt_total_vol","^total.*nekton.*ml",readr::parse_double,
                          df,"nekt_info","^types.*nekton",readr::parse_character,
                          df,"halo_num","^halo.*[^a-z]$",readr::parse_integer,
                          df,"gelat_num","^gelat.*[^a-z]$",readr::parse_integer,
                          df,"gelat_vol","^gelat.*ml",readr::parse_double,
                          df,"gelat_info","types.*gelat",readr::parse_character,
                          df,"sarg_natans_I","s.*natan.*.i.",readr::parse_double,
                          df,"sarg_natans_II","s.*natan.*.ii.",readr::parse_double,
                          df,"sarg_natans_VIII","s.*natan.*.viii.",readr::parse_double,
                          df,"sarg_fluitans_III","s.*flui.*.iii.",readr::parse_double,
                          df,"plas_pieces","^plastic.*pieces",readr::parse_integer,
                          df,"plas_pellets","^plastic.*pellet",readr::parse_integer,
                          df,"tar","^tar",readr::parse_integer
                          )

  namelist <- purrr::as_vector(dplyr::select(args,name))

  # Work out how to pass format arguments or just post-process afterward

  output <- purrr::pmap(dplyr::select(args,df,regex,parse_fun),parse_field)

  names(output) <- purrr::as_vector(dplyr::select(args,name))
  output <- tibble::as.tibble(output)

  # parse the datetime field
  output$date <- lubridate::as_date(output$date,origin="1900-1-1")
  local_in <- lubridate::as_datetime(output$time_in*60*60*24)
  date(local_in) <- output$date
  local_out <- lubridate::as_datetime(output$time_out*60*60*24)
  date(local_out) <- output$date
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

