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
    warning("One field not found in ELG file. Setting all values to NA")
    output <- parse_fun(rep(NA,nrow(df)),...)
  } else {
    output <- parse_fun(df[[ii]],...)
  }

}
