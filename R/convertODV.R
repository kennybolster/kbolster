#' Convert a data frame to a format suitable for importing into ODV
#'
#' This function replaces all NAs in a data frame with a replacement
#' value, by default -999
#' @param df Data frame to be converted
#' @param rep_value Value to replace NAs with. Defaults to -999
#' @export
#' @examples
#' convertODV(df)

convertODV <- function(df, rep_value = -999){
  # take a data frame df and replace all NA values with -999 or another
  # replacement value
  # example:
  # idp <- readRDS('idp.rds')
  # idp.odv <- convertODV(idp)
  newDf <- df
  newDf[is.na(df)] <- -999
  return(newDf)
}


