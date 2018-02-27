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

  df_names <- stringr::str_to_lower(names(df))

  # Find and parse field
  for (i in 1:length(regex)) {
    ii <- stringr::str_which(df_names,regex[i])
    if(length(ii)>0) break
  }

  if (length(ii)>1) {
    # warning(paste(name,"found in multiple slots:", paste0(ii,collapse="; "), ". By default, using data from slot", ii[1]))
    ii <- ii[1]
  }
  if (length(ii)==0) {
    warning("One field not found in ELG file. Setting all values to NA")
    # output <- parse_fun(df[[ii]],...)
    output <- rep(NA,nrow(df))
    # names(output) <- name
    # output <- list(parse_fun(rep(NA,nrow(df)),...))
    # names(output) <- name
    # df <- bind_cols(df,output)
  } else {
    # message(paste("Reading", name, "data from slot", ii))
    output <- parse_fun(df[[ii]],...)
    # names(output) <- name
    # names(df)[ii] <- name
    # df[[ii]] <- parse_fun(df[[ii]],...)
  }

  return(output)


}
