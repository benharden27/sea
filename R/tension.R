#' Find maximum tension for wire files in a folder
#'
#' @param data_folder the folder where the tension files are kept
#'
#' @return
#' @export
#'
#' @examples
max_tension <- function(data_folder, cruiseID = NULL, width = 8, thresh = 100, plotFL = TRUE) {

  # Create list of files to cycle through
  files <- list.files(data_folder,"^[CS]{1}[0-9]{3}")
  if(length(files)==0)
    stop("no files that meet criteria in folder")

  if(is.null(cruiseID))
    cruiseID <- stringr::str_extract(files[1],"^[CS][0-9]{3}")

  # create output data folder
  process_out <- file.path(data_folder,"processed")
  if (!file.exists(process_out))
    dir.create(process_out)

  # create output plotting folder
  if(plotFL) {
    plot_out <- file.path(process_out,"plots")
    if (!file.exists(plot_out))
      dir.create(plot_out)
  }

  # cycle through data files, find max tension and plot
  max_tension <- tibble::tibble(cast = files, max_tension = NA)
  for (i in 1:length(files)) {

    # read and process data
    file <- file.path(data_folder,files[i])
    data <- readr::read_csv(file, col_names = F, col_types = readr::cols(.default = readr::col_double()),skip = 10)
    data$X3 <- oce::despike(data$X3)
    datasm <- runmed(data$X3, 121, endrule = "constant")
    tr <- range(which(datasm > thresh), na.rm = T)
    tr <- round(tr + c(1,-1)*diff(tr)/width)
    max_tension$max_tension[i] = max(data$X3[tr[1]:tr[2]],na.rm = T)
    max_t_i <- tr[1] - 1 + which.max(data$X3[tr[1]:tr[2]])

    # plot data if required
    if(plotFL) {
      ggplot2::qplot(1:nrow(data),data$X3) +
      ggplot2::geom_point(ggplot2::aes(max_t_i, max_tension$max_tension[i]),
                          color = "red", size = 5) +
      ggplot2::ylab("Wire Tension") +
      ggplot2::xlab("Index")
      ggplot2::ggsave(file.path(plot_out, paste0(files[i], ".png")))
    }
  }

  readr::write_csv(max_tension, file.path(process_out, paste0(cruiseID, "_maxtension.csv")))

}
