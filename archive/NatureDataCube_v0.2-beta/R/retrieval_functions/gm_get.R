#' Get data from _GroenMonitor_
#'
#' Wrapper to get data from the _GroenMonitor_ WCS server.
#'
#' @import httr
#' @param url character. Request URL.
#' @param option character. Determines the type of request.
#' @param params vector or list. List of named parameters.
#' @returns A request response list.
#' @export

library(httr)

gm_get <- function(url, option = "NDVI", params,
                   out_path = paste(getwd(), tmpfile(), sep = "/")) {

  # Compose request URL
  if (missing(url) && !missing(option) && !missing(params)) {
    request_url <- gm_url(option = option, params = params)
  } else {
    request_url <- url
  }

  # Download file
  response <- GET(request_url, write_disk(out_path, overwrite = TRUE))

  return(response)
}
