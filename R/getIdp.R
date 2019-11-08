#' Download the GEOTRACES 2017 Intermediate Data Product
#'
#' The 2017 IDP, arranged in a useful format to work with in R, rather than
#' dealing with the files manually.
#' @return the IDP, a data frame/tibble
#' @export
#' @examples idp <- getIDP()

getIdp <- function(){
  idp <- readRDS(url(
    'https://github.com/kennybolster/kbolster/raw/master/idp.rds'))
  return(idp)
}
