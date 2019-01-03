#' Creates standard data structures of SEA data
#'
#' Takes data in standard SEA archive format and extracts the correct files
#' and creates standardized data format
#'
#' @param root_folder
#'
#' @return
#' @export
#'
#' @examples
package_data <- function(root_folder,cruiseID=NULL,...) {

  if(is.null(cruiseID)) {
    cruiseID <- tail(stringr::str_split(root_folder,'/')[[1]],1)
  }

  # start with ELG (is in top level directory)
  files <- list.files(root_folder,pattern = '\\.elg$')
  if(length(files) > 0) {
    for (i in 1:length(files)) {
      filein <- file.path(root_folder,files[i])
      if(i==1) {
        elg <- read_elg(filein, ...)
      } else {
        elg <- dplyr::bind_rows(elg,read_elg(filein, ...))
      }
    }
  } else {
    elg <- NULL
  }

  # then get datasheets
  files <- list.files(file.path(root_folder,"SHIPDATA"),pattern = "\\.xls")

  # hourly work
  hourly_file <- find_datasheet(files,"Hourlywork")
  if (length(hourly_file) == 1) {
    hourly <- try(read_hourly(file.path(root_folder,"SHIPDATA",hourly_file)))
    if(inherits(hourly,"try-error")) {
      warning("Hourly file cannot be opened. Returning NULL dataset.")
      hourly <- NULL
    }
  } else {
  hourly <- NULL
  }

  # surf station
  surfsamp_file <- find_datasheet(files,"surfsamp")
  if (length(surfsamp_file) == 1) {
    surfsamp <- read_surfsamp(file.path(root_folder,"SHIPDATA",surfsamp_file))
  } else {
    surfsamp <- NULL
  }

  # neuston
  neuston_file <- find_datasheet(files,"Neuston")
  if (length(neuston_file) == 1) {
    neuston <- read_neuston(file.path(root_folder,"SHIPDATA",neuston_file))
  } else {
    neuston <- NULL
  }

  # hydrowork
  hydro_file <- find_datasheet(files,"Hydrowork")
  if (length(hydro_file) == 1) {
    hydro <- read_hydrocast(file.path(root_folder,"SHIPDATA",hydro_file))
  } else {
    hydro <- NULL
  }

  # then adcp
  adcp_fold <-  file.path(root_folder,"ADCP")
  if(length(list.files(adcp_fold,".LTA")) == 0) {
    adcp_fold <- file.path(root_folder,"OceanDataView","ADCP Text Files")
    if(length(list.files(adcp_fold)) == 0) {
      adcp <- NULL
    } else {
      adcp <- read_adcp_fold(adcp_fold)
    }
  } else {
    adcp <- read_adcp_ens_fold(adcp_fold)
  }

  # then ctd
  ctd_fold <- file.path(root_folder,"CTD","Cnv")
  if(length(list.files(ctd_fold)) == 0) {
    ctd_fold <- file.path(root_folder,"cnv")
    if(length(list.files(ctd_fold)) == 0) {
      ctd <- NULL
    } else {
      ctd <- read_ctd_fold(ctd_fold)
    }
  } else {
    ctd <- read_ctd_fold(ctd_fold)
  }

  output <- list(elg = elg, hourly = hourly, surfsamp = surfsamp , neuston = neuston, hydro = hydro, adcp = adcp, ctd = ctd)

}


#' Find the name of a datasheet
#'
#' Takes in a list of files and a pattern and finds the best fit
#'
#' @param files
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
find_datasheet <- function(files,pattern) {
  data_file <- stringr::str_subset(files,pattern)
  if(length(data_file)>1){
    data_file <- data_file[which.min(nchar(data_file))]
  }
  return(data_file)
}


#' Convert a cnv file to a csv file
#'
#' @param in_file
#' @param out_fold
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
cnv_to_csv <- function(in_file,out_fold, overwrite = F, ...) {

  in_file_end <- tail(stringr::str_split(in_file,"/")[[1]],1)
  out_file <- stringr::str_replace(in_file_end,"\\.cnv","\\.csv")

  if(file.exists(file.path(out_fold,out_file)) & overwrite == F) {
    warning("File already exists in output, set overwrite=T to replace")
  } else {
    ctd <- read_ctd(in_file, ...)

    ctd_tib <- tibble::tibble(press = ctd@data$pressure,
                              depth = ctd@data$depth,
                              dens = ctd@data$sigmaTheta,
                              salt = ctd@data$salinity,
                              temp = ctd@data$theta,
                              O2 = ctd@data$oxygen,
                              fluor = ctd@data$fluorescence,
                              par = ctd@data$par)

    readr::write_csv(ctd_tib,file.path(out_fold,out_file))
  }
}


#' Convert all cnv files in a folder to csv
#'
#' @param in_fold
#' @param out_fold
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
cnv_to_csv_fold <- function(in_fold,out_fold, ...) {

  files <- list.files(in_fold,pattern = "\\.cnv")

  for (i in 1:length(files)) {
    cnv_to_csv(file.path(in_fold,files[i]), out_fold, ...)
  }

}



