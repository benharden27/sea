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

  # Convert all column names to lowercase
  df_names <- stringr::str_to_lower(names(df))

  # Find and parse field based on regex of column names
  for (i in 1:length(regex)) {
    ii <- stringr::str_which(df_names,regex[i])
    if(length(ii)>0) break
  }

  # if there are multiple matches return just the first value
  if(length(ii)>1) {
     ii <- ii[1]
  }

  # parse the selected column based on the parser given to function
  if (length(ii)==0) {
    warning(paste("One field not found in ELG file. Setting all values to NA:",regex[i]))
    output <- parse_fun(rep(NA,nrow(df)),...)
  } else {
    output <- parse_fun(df[[ii]],...)
  }

}


#' Parse lon from elg file
#'
#' @param lonin longitude to process
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



#' Parse a raw datasheet
#'
#' @param df
#' @param args
#'
#' @return
#' @export
#'
#' @examples
parse_datasheet <- function(df,args) {

  # create args to be passes to pmap
  args_in <- as_tibble(list(df=list(df),regex=args$regex,parse_fun=args$parse_fun))

  output <- purrr::pmap(args_in,parse_field)

  names(output) <- purrr::as_vector(dplyr::select(args,name))
  output <- tibble::as.tibble(output)

}
