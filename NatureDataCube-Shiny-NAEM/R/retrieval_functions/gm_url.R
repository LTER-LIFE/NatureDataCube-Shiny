#' Compose _GroenMonitor_ URL
#'
#' Compose URL text string to submit requests to the _GroenMonitor_ WCS server.
#'
#' @param option character. Determines the type of data.
#' @param params vector or list. List of named parameters.
#' @returns A request URL text string.
#' @export

gm_url <- function(option = "NDVI", params) {

  # Create the base URL
  base_url <- "https://data.groenmonitor.nl/geoserver/wcs?service=WCS&version=2.0.1"

  # Add parameters
  p <- lapply(na.omit(params), FUN = function(x) URLencode(x, repeated = TRUE))
  params_url <- paste("request=GetCoverage",
                      paste("coverageId=groenmonitor_",
                            tolower(option), p$date, sep = "_"),
                      paste0("subset=E(", p$xmin, ",", p$xmax, ")"),
                      paste0("subset=N(", p$ymin, ",", p$ymax, ")"),
                      paste0("format=image/", tolower(p$format)),
                      sep = "&")
  url <- paste0(base_url, "&", params_url)
  
  return(url)
}
