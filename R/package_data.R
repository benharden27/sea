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

  if(is_null(cruiseID)) {
    cruiseID <- tail(str_split(root_folder,'/')[[1]],1)
  }

  # start with ELG (is in top level directory)
  files <- list.files(root_folder,pattern = '\\.elg$')
  for (i in 1:length(files)) {
    filein <- file.path(root_folder,files[i])
    if(i==1) {
      elg <- read_elg(filein,...)
    } else {
      elg <- bind_rows(elg,read_elg(filein,...))
    }
  }

  # then get datasheets
  files <- list.files(file.path(root_folder,"SHIPDATA"),pattern = "\\.xls")

  # hourly work
  hourly_file <- find_datasheet(files,"Hourlywork")
  hourly <- read_hourly(file.path(root_folder,"SHIPDATA",hourly_file))

  # surf station
  surfsamp_file <- find_datasheet(files,"surfsamp")
  surfsamp <- read_surfsamp(file.path(root_folder,"SHIPDATA",surfsamp_file))

  # neuston
  neuston_file <- find_datasheet(files,"Neuston")
  neuston <- read_surfsamp(file.path(root_folder,"SHIPDATA",neuston_file))

  # hydrowork
  hydro_file <- find_datasheet(files,"Hydrowork")
  hydro <- read_surfsamp(file.path(root_folder,"SHIPDATA",hydro_file))

  output <- list(elg=elg,hourly=hourly,surfsamp=surfsamp,neuston=neuston,hydro=hydro)

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
  data_file <- str_subset(files,pattern)
  if(length(data_file)>1){
    data_file <- data_file[which.min(nchar(data_file))]
  }
  return(data_file)
}


