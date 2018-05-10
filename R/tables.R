#' Print a table of data
#'
#' @param dfo
#' @param caption
#' @param SigF
#' @param secondRow
#' @param thirdRow
#' @param save
#' @param fileout
#'
#' @return
#' @export
#'
#' @examples
print_table <- function(dfo, SigF = NULL, caption = NULL, hline.after = c(0),
                        secondRow = NULL, thirdRow = NULL,
                        save = FALSE, fileout = "table_output.tex", ...) {

  # Create the xtable
  out_table <- xtable::xtable(dfo, caption = caption)

  # align all colums to be centered
  xtable::align(out_table) <- rep("c", ncol(dfo)+1)

  # change number of Significant Figures if defined
  if(!is.null(SigF))
    xtable::digits(out_table) <- SigF

  # Add additional header rows as required
  add.to.row <- list(pos = list(0), command = NULL)

  if(!is.null(secondRow)) {
    secondRowAdd <- paste0(paste(secondRow, collapse=' & '), " \\\\ \n")
  } else {
    secondRowAdd <- NULL
  }
  if(!is.null(thirdRow)) {
    thirdRowAdd <- paste0(paste(thirdRow, collapse=' & '), " \\\\ \n")
  } else {
    thirdRowAdd <- NULL
  }

  # Comile the command for printing the table
  command <- paste0(secondRowAdd,
                    thirdRowAdd,
                    "\\hline\n\\endfirsthead\n",
                    paste0(paste(colnames(dfo), collapse=' & '), " \\\\ \n"),
                    secondRowAdd,
                    thirdRowAdd,
                    "\\hline\n\\endhead\n",
                    "\\hline\n",
                    "\\multicolumn{", ncol(dfo), "}{l}",
                    "{\\footnotesize Continued on next page}\n",
                    "\\endfoot\n",
                    "\\endlastfoot\n")

  add.to.row$command <- command

  # Print the table
  out <- xtable::print.xtable(out_table, hline.after=hline.after, add.to.row = add.to.row,
                              tabular.environment = "longtable",
                              floating = FALSE,
                              include.rownames = FALSE,
                              caption.placement = 'top',
                              comment = FALSE, ...)
  if(save) {
    fileout <- gsub(" ","",fileout)
    fileConn<-file(fileout)
    writeLines(out, fileConn)
    close(fileConn)
  }

}


#' Make a Hydrowork Table
#'
#' @param df
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
table_hydro <- function(df, caption = NULL, ...) {

  dfo <- dplyr::select(df,station, dttm, lon,lat, bottle, z, no3, po4, sio2, pH, alk, chla, temp, sal)
  dfo <- dplyr::mutate(dfo, dttm = format(dfo$dttm,"%Y-%m-%d %H:%M"))

  rep_station <- duplicated(dfo$station)
  dfo$station[rep_station] <- NA
  dfo$dttm[rep_station] <- NA
  dfo$lon[rep_station] <- NA
  dfo$lat[rep_station] <- NA

  hline_vec <- c(which(rep_station==F)-1,nrow(dfo))

  colnames(dfo) <- c('Station','Time',"Lon","Lat",'Bottle','Depth','NO3','PO4','Si02','pH','Alk','Chl-a','Temp','Sal')
  secondRow <- c('','(local)',"degN","degE","",'[m]','[uM]','[uM]','[uM]','','','[mg/L]','[degC]','')
  SigF <- c(20,20,2,2,20,0,2,2,2,2,2,3,1,2)

  emptyCols <- colSums(is.na(dfo)) == nrow(dfo)
  dfo<- dfo[!emptyCols]
  secondRow <- secondRow[!emptyCols]
  SigF <- SigF[!emptyCols]
  SigF <- append(20,SigF)

  print_table(dfo, SigF=SigF, secondRow=secondRow, caption = caption, hline.after = hline_vec, ...)

}

