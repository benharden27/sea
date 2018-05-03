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
    hourly <- read_hourly(file.path(root_folder,"SHIPDATA",hourly_file))
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
  adcp_fold <- file.path(root_folder,"OceanDataView","ADCP Text Files")
  if(length(list.files(adcp_fold)) == 0) {
    adcp <- NULL
  } else {
    adcp <- read_adcp_fold(adcp_fold)
  }


  output <- list(elg = elg, hourly = hourly, surfsamp = surfsamp , neuston = neuston, hydro = hydro, adcp = adcp)

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


